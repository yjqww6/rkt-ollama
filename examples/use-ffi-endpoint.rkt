#lang racket/base
(require "../main.rkt")
(provide use-ffi-endpoint)

(define (use-ffi-endpoint model ctx)
  (define init-model! (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'init-model!))
  (define completion (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'completion))
  (define mc (init-model! #:path model #:context ctx))
  (current-completion-endpoint
   (λ (prompt output)
     (completion mc prompt output #:n-predict (current-num-predict)
                 #:perf (λ (p pp ppt tg tgt)
                          (log-perf-trace (perf p tg (/ ppt 1000) (/ tgt 1000)
                                                (/ pp (/ ppt 1000)) (/ tg (/ tgt 1000)))))))))