#lang racket/base
(require "private/main.rkt" "private/config.rkt" "private/log.rkt"
         racket/generator)
(provide chat generate chat/history chat/output generate/output
         close-response response? embeddings list-models
         (all-from-out "private/config.rkt" "private/log.rkt")
         chat/gen)

(define (chat/gen message proc
                  #:assistant-start [fake #f]
                  #:yield-json? [yield-json? #f])
  (define prompt (make-continuation-prompt-tag))
  (call-with-continuation-prompt
   (λ ()
     (chat/history
      message
      (λ (content json)
        (yield (if yield-json? json content)))
      #:before
      (λ ()
        (call-with-composable-continuation
         (λ (cc)
           (abort-current-continuation
            prompt
            (λ () (proc (generator () (cc))))))
         ))
      #:assistant-start fake)
     (yield eof))
   prompt))