#lang racket/base
(require racket/match)
(provide (all-defined-out))

(define current-system (make-parameter #f))
(define current-history (make-parameter '()))

(define (save-history [path "history.rktd"])
  (call-with-output-file path
    (λ (p) (write (hasheq 'system (current-system) 'history (current-history)) p))))

(define (restore-history [path "history.rktd"])
  (call-with-input-file
      path
    (λ (p)
      (match (read p)
        [(hash* ['system s] ['history h])
         (current-system s)
         (current-history h)]))))

(define (make-system system)
  (and system (hasheq 'role "system" 'content system)))

(define (fake-assistant fake)
  (and fake (hasheq 'role "assistant" 'content fake)))

(define (make-user content [images #f])
  (define msg (hasheq 'role "user" 'content content))
  (if images (hash-set msg 'images images) msg))

(define (append-history . args)
  (let f ([args args])
    (match args
      [(cons (? list? h) r) (append h (f r))]
      [(cons #f r) (f r)]
      [(cons m r) (cons m (f r))]
      ['() '()])))

(define (undo)
  (let loop ([ls (reverse (current-history))])
    (match ls
      ['() (void)]
      [(cons (hash* ['role "user"]) r) (current-history (reverse r))]
      [(cons m r) (loop r)])))

(define (shift)
  (current-history
   (let loop ([h (current-history)])
     (match h
       ['() '()]
       [(cons (hash* ['role "assistant"]) r) r]
       [(cons m r) (loop r)]))))

(define (clear)
  (current-history '()))

;;; for raw completion
(define current-chat-template (make-parameter (λ (messages) (error 'empty-template))))
(define current-messages-preprocessor (make-parameter values))
