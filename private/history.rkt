#lang racket/base
(require racket/match racket/bool racket/string racket/list)
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
      (match-define (hash* ['system s] ['history h]) (read p))
      (current-system s)
      (current-history h))))

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
      [(cons (hash* ['role (not "assistant")]) r) (current-history (reverse r))]
      [(cons m r) (loop r)])))

(define (history-pop)
  (current-history
   (let loop ([h (current-history)])
     (match h
       ['() '()]
       [(cons (hash* ['role "assistant"]) r) r]
       [(cons m r) (loop r)]))))

(define (clear)
  (current-history '()))

(define current-skip-cot-tokens (make-parameter #t))
(define (skip-cot-tokens msgs [sep "</think>"] #:skip-current? [skip-current? #t])
  (cond
    [(current-skip-cot-tokens)
     (define end (length msgs))
     (for/list ([m (in-list msgs)]
                [i (in-naturals 1)])
       (match m
         [(hash 'role "assistant" 'content content #:open)
          #:when (implies skip-current? (< i end))
          #:when (string-contains? content sep)
          (hash-set m 'content
                    (string-trim (last (string-split content sep)) "\n"
                                 #:right? #f #:repeat? #t))]
         [else m]))]
    [else msgs]))

(define current-messages-preprocessor (make-parameter skip-cot-tokens))

;;; for raw completion
(define (default-template-postprocessor parts)
  (apply string-append-immutable (map (λ (s) (if (box? s) (unbox s) s)) parts)))
(define current-chat-template (make-parameter (λ (messages) (error 'empty-template))))
(define current-template-postprocessor (make-parameter default-template-postprocessor))

