#lang racket/base
(require "config.rkt" "history.rkt" "log.rkt"
         racket/match racket/generator net/http-client json)
(provide  generate chat response? close-response
          chat/history/output chat/history generate/output
          embeddings list-models)

(struct response (port)
  #:property prop:sequence
  (λ (resp)
    (in-port
     (λ (p)
       (define l (read-line p))
       (cond
         [(eof-object? l) l]
         [else
          (log-network-trace (network:recv l))
          (define j (string->jsexpr l))
          (perf-trace j)
          j]))
     (response-port resp))))

(struct response/producer response (proc)
  #:property prop:sequence
  (λ (resp)
    (in-producer (response/producer-proc resp) eof-object?)))

(define (close-response resp)
  (cond
    [(response-port resp) => close-input-port]))

(define (send path j #:method [method "POST"])
  (define data (and j (jsexpr->bytes j)))
  (when data
    (log-network-trace (network:send data)))
  (define-values (status headers chat-port)
    (http-sendrecv
     (current-host) path
     #:port (current-port) #:method method
     #:headers '("Content-Type: application/json")
     #:data data))
  chat-port)

(define (chat messages)
  (define data
    (hash-param
     'model (current-model)
     'messages messages
     'tools (current-tools)
     'stream (box (current-stream))
     'options (current-options)
     'format (current-response-format)))
  (response (send "/api/chat" data)))

(define (call/interupt proc empty)
  (with-handlers* ([(λ (e) (and (exn:break? e)
                                (continuation-prompt-available? break-prompt-tag)))
                    (λ (e)
                      (call/cc
                       (λ (cc)
                         (abort-current-continuation break-prompt-tag cc)))
                      (empty))])
    (proc)))

(define (call/history message proc #:assistant-start [fake #f])
  (define messages
    (append-history 
     (make-system (current-system))
     (current-history)
     message
     (fake-assistant fake)))
  (define sp (open-output-string))
  (when fake
    (write-string fake sp))
  (define result (proc messages sp))
  (current-history
   (append-history
    (current-history)
    message
    (hash-set (hash-ref result 'message)
              'content (get-output-string sp)))))

(define (chat-with-history message proc #:before [before #f] #:after [after #f]
                           #:assistant-start [fake #f])
  (call/history
   message
   (λ (messages sp)
     (define resp (chat messages))
     (when before (before resp))
     (begin0
       (call/interupt
        (λ ()
          (for/last ([j resp]
                     #:final (hash-ref j 'done #f))
            (match j
              [(hash* ['message (hash* ['content content])])
               (write-string content sp)
               (proc content j)
               j]
              [(hash* ['error err])
               (error 'chat/history "~a" err)])))
        (λ () (hasheq 'message (fake-assistant ""))))
       (when after (after resp))))
   #:assistant-start fake))

(define (chat/history/output message output
                             #:assistant-start [fake #f])
  (chat-with-history
   message
   #:before (and fake (λ (resp) (write-string fake output)))
   (λ (content json)
     (write-string content output)
     (flush-output output))
   #:after close-response
   #:assistant-start fake))

(define (chat/history message
                      #:assistant-start [fake #f]
                      #:json? [json? #f]
                      #:close-response? [close-response? #t])
  (define prompt (make-continuation-prompt-tag))
  (call-with-continuation-prompt
   (λ ()
     (chat-with-history
      message
      #:before
      (λ (resp)
        (call-with-composable-continuation
         (λ (cc)
           (abort-current-continuation
            prompt
            (λ () (response/producer
                   (response-port resp)
                   (generator () (cc))))))))
      (λ (content json)
        (yield (if json? json content)))
      #:after (and close-response? close-response)
      #:assistant-start fake)
     (yield eof))
   prompt))

(define (generate prompt
                  #:images [images #f]
                  #:template [template #f]
                  #:stream? [stream? #t]
                  #:raw? [raw #f]
                  #:context [context #f])
  (define data
    (hash-param 'model (current-model)
                'prompt prompt
                'system (current-system)
                'images images
                'options (current-options)
                'stream (box (current-stream))
                'raw raw
                'template template
                'context context
                'format (current-response-format)))
  (response (send "/api/generate" data)))

(define (generate/output prompt output
                         #:images [images #f]
                         #:template [template #f]
                         #:stream? [stream? #t]
                         #:raw? [raw #f]
                         #:context [context #f])
  (define resp (generate prompt
                         #:images images
                         #:template template
                         #:stream? stream?
                         #:raw? raw
                         #:context context))
  (define result
    (for/last ([j resp])
      (match j
        [(hash* ['done done] ['response content])
         (write-string content output)
         (flush-output output)
         j]
        [(hash* ['error err])
         (error 'generate "~a" err)])))
  (close-response resp)
  result)

(define (perf-trace j)
  (match j
    [(hash* ['total_duration total_duration]
            ['load_duration load_duration]
            ['prompt_eval_duration prompt_eval_duration]
            ['eval_duration eval_duration]
            ['prompt_eval_count prompt_eval_count]
            ['eval_count eval_count])
     (log-perf-trace (perf prompt_eval_count eval_count (/ prompt_eval_duration 1e9) (/ eval_duration 1e9) #f #f))]
    [else (void)]))

(define (embeddings prompt)
  (define data
    (hash-param
     'model (current-model)
     'input prompt))
  (define chat-port (send "/api/embed" data))
  (define j (begin0 (read-json chat-port) (close-input-port chat-port)))
  (log-network-trace (network:recv j))
  (hash-ref j 'embeddings))

(define (list-models [detailed? #f])
  (define chat-port (send "/api/tags" #f #:method "GET"))
  (define j (begin0 (read-json chat-port) (close-input-port chat-port)))
  (log-network-trace (network:recv j))
  (define models (hash-ref j 'models))
  (cond
    [detailed? models]
    [else (for/list ([m (in-list models)])
            (hash-ref m 'model))]))

(module+ llama-cpp
  (require racket/string)
  (provide chat/history/output completion/output current-options current-grammar
           current-chat-template completion/history/output)
  (define current-options (make-parameter (hasheq 'cache_prompt #t)))
  (define current-grammar (make-option 'grammar current-options))

  (define (build-options)
    (hash-param
     'stream (box (current-stream))
     'temperature (current-temperature)
     'top_k (current-top-k)
     'top_p (current-top-p)
     'n_predict (current-num-predict)
     (current-options)))

  (define ((reciever port))
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
  
  (define (chat messages)
    (define data
      (hash-set (build-options) 'messages messages))
    (define p
      (send "/v1/chat/completions" data))
    (response/producer p (reciever p)))

  (define (chat/history/output message output)
    (define messages
      (append-history
       (make-system (current-system))
       (current-history)
       message))
    (define sp (open-output-string))
    (define resp (chat messages))
    (let/ec k
      (call/interupt
       (λ ()
         (for ([j resp])
           (match j
             [(hash* ['choices (list (or (hash* ['delta (hash* ['content content])])
                                         (hash* ['message (hash* ['content content])])) _ ...)])
              (write-string content sp)
              (write-string content output)
              (flush-output output)]
             [(hash* ['choices (list (hash* ['finish_reason (? string?)]) _ ...)])
              (match j
                [(hash* ['usage (hash* ['completion_tokens eval_count] ['prompt_tokens prompt_eval_count])])
                 (log-perf-trace (perf prompt_eval_count eval_count #f #f #f #f))]
                [else (void)])
              (k)]
             [(hash* ['error _])
              (error 'chat "~a" j)])))
       void))
    (close-response resp)
    (current-history
     (append-history
      (current-history)
      message
      (hasheq 'role "assistant"
              'content (get-output-string sp)))))

  (define (completion/output prompt output)
    (define data (hash-set (build-options) 'prompt prompt))
    (define p (send "/completion" data))
    (define resp (response/producer p (reciever p)))
    (let/ec k
      (for ([j resp])
        (match j
          [(hash* ['content content] ['stop stop])
           (write-string content output)
           (flush-output output)
           (when stop
             (match j
               [(hash* ['tokens_evaluated prompt-tokens] ['tokens_predicted eval-tokens]
                       ['timings (hash* ['prompt_ms prompt-duration] ['predicted_ms eval-duration]
                                        ['prompt_per_second prompt-tokens-per-second]
                                        ['predicted_per_second eval-tokens-per-seoncd])])
                (log-perf-trace (perf prompt-tokens eval-tokens (/ prompt-duration 1e3) (/ eval-duration 1e3)
                                      prompt-tokens-per-second eval-tokens-per-seoncd))])
             (k))])))
    (close-response resp))

  (define current-chat-template (make-parameter (λ (messages) (error 'empty-template))))
  
  (define (completion/history/output message output #:assistant-start [fake #f] #:template [template (current-chat-template)])
    (call/history
     message
     (λ (messages sp)
       (define prompt (template messages))
       (define data (hash-set (build-options) 'prompt prompt))
       (define p (send "/completion" data))
       (define resp (response/producer p (reciever p)))
       (when fake
         (write-string fake output))
       (let/ec k
         (for ([j resp])
           (match j
             [(hash* ['content content] ['stop stop])
              (write-string content sp)
              (write-string content output)
              (flush-output output)
              (when stop
                (match j
                  [(hash* ['tokens_evaluated prompt-tokens] ['tokens_predicted eval-tokens]
                          ['timings (hash* ['prompt_ms prompt-duration] ['predicted_ms eval-duration]
                                           ['prompt_per_second prompt-tokens-per-second]
                                           ['predicted_per_second eval-tokens-per-seoncd])])
                   (log-perf-trace (perf prompt-tokens eval-tokens (/ prompt-duration 1e3) (/ eval-duration 1e3)
                                         prompt-tokens-per-second eval-tokens-per-seoncd))])
                (k))])))
       (close-response resp)
       (hasheq 'message (hasheq 'role "assistant")))
     #:assistant-start fake)))
