#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define)
(provide (all-defined-out))
(define hash-param
  (case-lambda
    [() (hasheq)]
    [(k v . r)
     (define h (apply hash-param r))
     (cond
       [(box? v) (hash-set h k (unbox v))]
       [v (hash-set h k v)]
       [else h])]))

(define-syntax-parse-rule (define-options C [Param Key] ...+)
  (begin
    (define Param (make-parameter #f))
    ...
    (define C
      (case-lambda
        [()
         (hash-param
          (~@ 'Key (Param))
          ...)]
        [(opt)
         (Param (and opt (hash-ref opt 'Key #f)))
         ...]))))

(define current-model (make-parameter "gemma2"))
(define current-system (make-parameter #f))
(define current-tools (make-parameter #f))
(define current-stream (make-parameter #t))

(define current-host (make-parameter "localhost"))
(define current-port (make-parameter 11434))
(define current-verbose (make-parameter #f))

(define-options current-options
  [current-context-window num_ctx]
  [current-temperature temperature]
  [current-repeat-penalty repeat_penalty]
  [current-top-p top_p]
  [current-seed seed])