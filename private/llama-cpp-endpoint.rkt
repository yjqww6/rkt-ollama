#lang racket/base
(require "config.rkt" "common.rkt" "log.rkt" "history.rkt")
(require racket/string racket/match racket/port json)
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
   'min_p (current-min-p)
   'repeat_penalty (current-repeat-penalty)
   'repeat_last_n (current-repeat-last-n)
   'max_tokens (current-num-predict)
   'stop (current-stop)
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
  (call/history
   message
   (λ (messages sp)
     (define resp (chat messages))
     (let/ec k
       (call/interrupt
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
     (hasheq 'message (hasheq 'role "assistant")))))

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
     (completion/output prompt (combine-output sp output))
     (hasheq 'message (hasheq 'role "assistant")))
   #:assistant-start fake))