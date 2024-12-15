#lang racket/base
(require "config.rkt" "common.rkt" "log.rkt" "history.rkt" "json.rkt")
(require racket/string racket/match)
(provide llama-cpp-chat-endpoint llama-cpp-completion-endpoint)

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

(define (log-perf j)
  (match j
    [(hash* ['timings (hash* ['prompt_n prompt-tokens] ['predicted_n eval-tokens]
                             ['prompt_ms prompt-duration] ['predicted_ms eval-duration]
                             ['prompt_per_second prompt-tokens-per-second]
                             ['predicted_per_second eval-tokens-per-seoncd])])
     (log-perf-trace (perf prompt-tokens eval-tokens (/ prompt-duration 1e3) (/ eval-duration 1e3)
                           prompt-tokens-per-second eval-tokens-per-seoncd))]
    [else (void)]))

(define (handle-chat-response resp output)
  (let/ec k
    (for ([j resp])
      (log-resp-trace j)
      (log-perf j)
      (match j
        [(hash* ['choices (list (or (hash* ['delta (hash* ['content content])])
                                    (hash* ['message (hash* ['content content])])) _ ...)])
         (write-string content output)
         (flush-output output)]
        [(hash* ['choices '()]) (void)]
        [(hash* ['choices (list (hash* ['finish_reason (? string?)]) _ ...)])
         (k)]
        [(hash* ['error _])
         (error 'chat "~a" j)]
        [else (void)]))))

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
     (handle-chat-response resp output)
     (close-response resp))))

(define (handle-completion-response resp output)
  (let/ec k
    (for ([j resp])
      (log-resp-trace j)
      (log-perf j)
      (match j
        [(hash* ['content content] ['stop stop])
         (write-string content output)
         (flush-output output)
         (when stop (k))]))))

(define (completion prompt)
  (define data (hash-set (build-options) 'prompt prompt))
  (define p (send "/completion" data))
  (response/producer p (reciever p)))

(define (llama-cpp-completion-endpoint prompt output)
  (define resp (completion prompt))
  (handle-completion-response resp output)
  (close-response resp))
