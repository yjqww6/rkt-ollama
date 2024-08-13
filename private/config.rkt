#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define)
(provide (all-defined-out))
(define hash-param
  (case-lambda
    [() (hasheq)]
    [(h) (or h (hasheq))]
    [(k v . r)
     (define h (apply hash-param r))
     (cond
       [(box? v) (hash-set h k (unbox v))]
       [v (hash-set h k v)]
       [else h])]))

(define current-model (make-parameter "gemma2"))
(define current-system (make-parameter #f))
(define current-tools (make-parameter #f))
(define current-stream (make-parameter #t))
(define current-response-format (make-parameter #f))

(define current-host (make-parameter "localhost"))
(define current-port (make-parameter 11434))
(define current-verbose (make-parameter #f))
(define break-prompt-tag (make-continuation-prompt-tag))

(define current-options (make-parameter #f))

(define (make-option key)
  (make-derived-parameter
   current-options
   (λ (v) (hash-set (or (current-options) (hasheq)) key v))
   (λ (v) (cond
            [(current-options)
             =>
             (λ (c) (hash-ref c key #f))]
            [else #f]))))

(define-syntax-parse-rule (define-options [Name:id Key:id] ...)
  (begin (define Name (make-option 'Key)) ...))

(define-options
  [current-context-window num_ctx]
  [current-temperature temperature]
  [current-repeat-penalty repeat_penalty]
  [current-top-p top_p]
  [current-seed seed])
