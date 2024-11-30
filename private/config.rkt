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

(define current-model (make-parameter #f))
(define default-system (make-parameter #f))
(define current-tools (make-parameter #f))
(define current-stream (make-parameter #t))

(define default-endpoint (make-parameter (cons "localhost" 11434)))

(define current-host (make-derived-parameter
                      (make-parameter #f)
                      values
                      (λ (v) (or v (car (default-endpoint))))))
(define current-port (make-derived-parameter
                      (make-parameter #f)
                      values
                      (λ (v) (or v (cdr (default-endpoint))))))
(define current-verbose (make-parameter #f))
(define break-prompt-tag (make-continuation-prompt-tag))

(define current-options (make-parameter #f))

(define-syntax-parse-rule (define-options [Name:id (~optional Def:expr #:defaults ([Def #'#f]))] ...)
  (begin (define Name (make-parameter Def)) ...))

(define-options
  [current-context-window 8192]
  [current-temperature]
  [current-repeat-penalty]
  [current-repeat-last-n]
  [current-top-p]
  [current-min-p]
  [current-top-k]
  [current-seed]
  [current-num-predict]
  [current-stop]
  [current-enforce-json]
  ;; llama.cpp only
  [current-grammar]
  [current-json-schema])