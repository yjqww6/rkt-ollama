#lang racket/base
(require racket/match)
(provide (all-defined-out))

(define current-history (make-parameter '()))

(define (save-history [path "history.rktd"])
  (call-with-output-file path (λ (p) (write (current-history) p))))

(define (restore-history [path "history.rktd"])
  (call-with-input-file path (λ (p) (current-history (read p)))))

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

(define (use-system sys)
  (current-history (prepend-system sys (current-history) #:replace? #t)))

(define (prepend-system sys history #:replace? replace?)
  (match history
    [(cons (hash* ['role "system"]) h)
     (if (not replace?)
         history
         (if sys (cons (make-system sys) h) h))]
    [h
     (if sys (cons (make-system sys) h) h)]))

(define (undo)
  (let loop ([ls (reverse (current-history))])
    (match ls
      ['() (void)]
      [(cons (hash* ['role "user"]) r) (current-history (reverse r))]
      [(cons m r) (loop r)])))

(define (shift)
  (define (f h)
    (let loop ([h h])
      (match h
        ['() '()]
        [(cons (hash* ['role "assistant"]) r) r]
        [(cons m r) (loop r)])))
  (current-history
   (match (current-history)
     [(cons (and s (hash* ['role "system"])) h)
      (cons s (f h))]
     [h (f h)])))

(define (clear)
  (current-history '())
  (current-context #f))

(define current-context (make-parameter #f))
