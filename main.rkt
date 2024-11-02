#lang racket/base
(require "private/config.rkt" "private/history.rkt" "private/image.rkt" "private/log.rkt"
         syntax/parse/define
         racket/string racket/match)
(provide (all-from-out "private/config.rkt" "private/history.rkt" "private/log.rkt")
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

(module+ ollama
  (require "private/main.rkt")
  (provide chat completion)
  (define (chat #:output [output (current-chat-output-port)]
                #:start [fake (current-assistant-start)]
                . items)
    (with-cust _
      (chat/history/output (build-message "user" items) output #:assistant-start fake)
      (void)))

  (define (completion #:output [output (current-chat-output-port)]
                    . items)
    (define-values (prompt images) (collect items))
    (with-cust _
      (generate/output prompt output #:images images #:raw? #t)
      (void))))

(module+ llama-cpp
  (require "private/llama-cpp-endpoint.rkt")
  (provide chat completion chat-by-completion (all-from-out "private/llama-cpp-endpoint.rkt"))
  
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
