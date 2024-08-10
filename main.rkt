#lang racket/base
(require "private/config.rkt" "private/history.rkt" "private/image.rkt" "private/log.rkt"
         (prefix-in p: "private/main.rkt")
         syntax/parse/define
         racket/string racket/match)
(provide (all-from-out "private/config.rkt" "private/history.rkt" "private/log.rkt")
         chat generate undo redo continue clear preload
         current-chat-output-port current-assistant-start
         with-cust)

(define (call-with-cust thunk)
  (let ([cust (make-custodian)])
    (dynamic-wind
     void
     (λ ()
       (parameterize ([current-custodian cust])
         (thunk cust)))
     (λ () (custodian-shutdown-all cust)))))

(define-syntax-parse-rule (with-cust C:id Body:expr ...+)
  (call-with-cust (λ (C) Body ...)))

(define (collect items)
  (let loop ([items items] [strs '()] [imgs '()])
    (cond
      [(null? items) (values (string-join (reverse strs) "\n")
                             (and (not (null? imgs))
                                  (map get-image (reverse imgs))))]
      [(string? (car items)) (loop (cdr items) (cons (car items) strs) imgs)]
      [(list? (car items)) (loop (append (car items) (cdr items)) strs imgs)]
      [(not (car items)) (loop (cdr items) strs imgs)]
      [else (loop (cdr items) strs (cons (car items) imgs))])))

(define (build-message role items)
  (define-values (content images) (collect items))
  (hash-param 'role role
              'content content
              'images images))

(define current-chat-output-port
  (make-derived-parameter
   (make-parameter #f)
   values
   (λ (v) (or v (current-output-port)))))

(define current-assistant-start (make-parameter #f))

(define (chat #:output [output (current-chat-output-port)]
              #:start [fake (current-assistant-start)]
              . items)
  (with-cust _
    (p:chat/output (build-message "user" items) output #:assistant-start fake)
    (void)))

(define (generate #:output [output (current-chat-output-port)] . items)
  (define-values (prompt images) (collect items))
  (with-cust _
    (p:generate/output prompt output #:images images)
    (void)))

(define (redo #:output [output (current-chat-output-port)] #:start [fake #f])
  (match-define (list history ... user assistant) (current-history))
  (current-history
   (parameterize ([current-history history])
     (p:chat/output user output #:assistant-start fake)
     (current-history))))

(define (continue #:output [output (current-chat-output-port)])
  (match-define (list history ... user (hash* ['role "assistant"] ['content fake]))
    (current-history))
  (current-history
   (parameterize ([current-history history])
     (p:chat/output user output #:assistant-start fake)
     (current-history))))

(define (preload)
  (parameterize ([current-tools #f])
    (p:chat '())
    (void)))
