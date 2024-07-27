#lang racket/base
(require racket/match)
(provide (all-defined-out))

(define current-history (make-parameter '()))

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

(define (clear)
  (current-history '()))
