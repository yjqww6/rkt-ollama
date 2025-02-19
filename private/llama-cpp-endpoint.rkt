#lang racket/base
(require "common.rkt"
         "config.rkt"
         "history.rkt"
         "json.rkt"
         "log.rkt")
(require racket/match
         racket/string
         racket/port)
(provide llama-cpp-chat-endpoint llama-cpp-completion-endpoint llama-cpp-tokenize-endpoint)

(define (build-options)
  (hash-param
   'model (current-model)
   'stream (box (current-stream))
   'stream_options (and (current-stream) (hasheq 'include_usage #t))
   'temperature (current-temperature)
   'top_k (current-top-k)
   'top_p (current-top-p)
   'min_p (current-min-p)
   'repeat_penalty (current-repeat-penalty)
   'repeat_last_n (current-repeat-last-n)
   'max_tokens (current-num-predict)
   'stop (current-stop)
   'grammar (current-grammar)
   'json_schema (or (current-json-schema)
                    (and (current-enforce-json) (hasheq)))
   'cache_prompt #t
   (current-options)))

(struct response:llama-cpp response ()
  #:property prop:sequence
  (λ (resp)
    (define port (response-port resp))
    (in-producer
     (λ ()
       (let loop ()
         (define l (read-line port 'any-one))
         (when (non-empty-string? l)
           (log-network-trace (network:recv l)))
         (cond
           [(eof-object? l) l]
           [(not (non-empty-string? l)) (loop)]
           [(not (string-prefix? l "data: ")) (string->jsexpr l)]
           [(string=? l "data: [DONE]") eof]
           [else (string->jsexpr (substring l 5))])))
     eof-object?)))

(struct response:json response ()
  #:property prop:sequence
  (λ (resp)
    (define s (port->string (response-port resp) #:close? #t))
    (log-network-trace (network:recv s))
    (in-value
     (string->jsexpr s))))

(define TYPE-REGEXP (byte-regexp (bytes-append #"^(?i:" (regexp-quote #"Content-Type") #"): ")))
(define TYPE-JSON-REGEXP (byte-regexp (bytes-append  #"(?i:" (regexp-quote #"application/json") #")")))

(define (chat messages)
  (define data
    (hash-param 'messages messages
                'tools (current-tools)
                (build-options)))
  (define-values (p headers)
    (send "/v1/chat/completions" data #:return-headers #t))
  (define whole?
    (for/or ([h (in-list headers)])
      (and (regexp-match? TYPE-REGEXP h)
           (regexp-match? TYPE-JSON-REGEXP h))))
  (if whole?
      (response:json p)
      (response:llama-cpp p)))

(define (log-perf j)
  (match j
    [(hash* ['timings (hash* ['prompt_n prompt-tokens] ['predicted_n eval-tokens]
                             ['prompt_ms prompt-duration] ['predicted_ms eval-duration]
                             ['prompt_per_second prompt-tokens-per-second]
                             ['predicted_per_second eval-tokens-per-seoncd])])
     (log-perf-trace (perf prompt-tokens eval-tokens (/ prompt-duration 1e3) (/ eval-duration 1e3)
                           prompt-tokens-per-second eval-tokens-per-seoncd))]
    [else (void)]))

(define (merge-tool-call a b)
  (match* (a b)
    [((hash* ['id (? string? ida) #:default ""]
             ['function
              (hash*
               ['name (? string? namea) #:default ""]
               ['arguments (? string? argumentsa) #:default ""])])
      (hash* ['id (? string? idb) #:default ""]
             ['function
              (hash*
               ['name (? string? nameb) #:default ""]
               ['arguments (? string? argumentsb) #:default ""])]))
     (hash 'id (string-append ida idb)
           'type "function"
           'function
           (hash
            'name (string-append namea nameb)
            'arguments (string-append argumentsa argumentsb)))]
    [((? hash?) #f)  a]
    [(#f (? hash?)) b]))

(define (handle-chat-response resp output tool-calls-output)
  (define stream-tools (make-hash))
  (let/ec k
    (for ([j resp])
      (log-resp-trace j)
      (log-perf j)
      (match j
        [(hash 'choices (list (hash 'message (hash 'tool_calls tool-calls #:open) #:open)) #:open)
         (tool-calls-output tool-calls)]
        [(hash* ['choices (list (hash* ['delta (hash* ['tool_calls tool-calls])]))])
         (for ([tc (in-list tool-calls)])
           (define index (hash-ref tc 'index (λ () 0)))
           (define a (hash-ref stream-tools index (λ () #f)))
           (hash-set! stream-tools index (merge-tool-call a tc)))]
        [else (void)])
      (match j
        [(hash* ['choices (list (or (hash* ['delta (hash* ['content (? string? content)])])
                                    (hash* ['message (hash* ['content (? string? content)])])) _ ...)])
         (write-string content output)
         (flush-output output)]
        [(hash* ['choices '()]) (void)]
        [(hash* ['choices (list (hash* ['finish_reason (? string?)]) _ ...)])
         (k)]
        [(hash* ['error _])
         (error 'chat "~a" j)]
        [else (void)])))
  (unless (hash-empty? stream-tools)
    (tool-calls-output (hash-values stream-tools))))

(define (call/prefill-workaround messages proc)
  (match messages
    [(list m ... (hash 'role "assistant" 'content fake))
     (parameterize ([current-grammar (format "root ::= ~v .*" fake)])
       (proc m))]
    [else
     (proc messages)]))

(define (llama-cpp-chat-endpoint messages output [fake #f] [tool-calls-output void])
  (call/prefill-workaround
   messages
   (λ (messages)
     (define resp (chat messages))
     (handle-chat-response resp output tool-calls-output)
     (close-response resp))))

(define (handle-completion-response resp output)
  (let/ec k
    (for ([j resp])
      (log-resp-trace j)
      (log-perf j)
      (match-define (hash* ['content content] ['stop stop]) j)
      (write-string content output)
      (flush-output output)
      (when stop
        (k)))))

(define (completion prompt)
  (define data (hash-set (build-options) 'prompt prompt))
  (define p (send "/completion" data))
  (response:llama-cpp p))

(define (llama-cpp-completion-endpoint prompt output)
  (define resp (completion prompt))
  (handle-completion-response resp output)
  (close-response resp))

(define (llama-cpp-tokenize-endpoint prompt)
  (define p (send "/tokenize" (hasheq 'content prompt)))
  (define result (read-json p))
  (close-input-port p)
  (hash-ref result 'tokens))
