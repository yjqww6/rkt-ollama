#lang racket/base
(require "private/config.rkt" "private/history.rkt" "private/image.rkt" "private/log.rkt"
         (prefix-in p: "private/main.rkt")
         syntax/parse/define
         racket/string racket/match)
(provide (all-from-out "private/config.rkt" "private/history.rkt" "private/log.rkt")
         chat generate undo clear
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
  (match items
    [(list (? hash? h)) h]
    [else
     (define-values (content images) (collect items))
     (hash-param 'role role
                 'content content
                 'images images)]))

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
    (p:chat/history/output (build-message "user" items) output #:assistant-start fake)
    (void)))

(define (generate #:output [output (current-chat-output-port)] #:raw? [raw #f] #:use-context? [use-context #f]
                  . items)
  (define-values (prompt images) (collect items))
  (with-cust _
    (define r (p:generate/output prompt output #:images images #:raw? raw
                                 #:context (and use-context (current-context))))
    (when use-context
      (match r
        [(hash* ['context new-ctx])
         (current-context new-ctx)]
        [else (void)]))
    (void)))

(module+ llama-cpp
  (require (submod "private/main.rkt" llama-cpp))
  (provide chat completion chat-by-completion (all-from-out (submod "private/main.rkt" llama-cpp)))
  
  (define (call/prefill-workaround fake f)
    (cond
      [(not fake) (f)]
      [else
       (parameterize ([current-grammar (format "root ::= ~v .*" fake)])
         (f))]))
  
  (define (chat #:output [output (current-chat-output-port)]
                #:start [fake (current-assistant-start)]
                . items)
    (define (f)
      (with-cust _
        (chat/history/output (build-message "user" items) output)
        (void)))
    (call/prefill-workaround fake f))
  
  (define (completion #:output [output (current-chat-output-port)]
                      #:start [fake (current-assistant-start)]
                      . items)
    (define (f)
      (define-values (prompt images) (collect items))
      (with-cust _
        (completion/output prompt output)
        (void)))
    (call/prefill-workaround fake f))

  (define (chat-by-completion
           #:output [output (current-chat-output-port)]
           #:start [fake (current-assistant-start)]
           . items)
    (with-cust _
      (completion/history/output (build-message "user" items)
                                 output
                                 #:assistant-start fake)
      (void))))
