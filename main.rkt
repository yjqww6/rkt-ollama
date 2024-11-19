#lang racket/base
(require "private/config.rkt" "private/history.rkt" "private/image.rkt" "private/log.rkt"
         "private/common.rkt"
         syntax/parse/define
         racket/string racket/match)
(provide (all-from-out "private/config.rkt" "private/history.rkt" "private/log.rkt")
         current-chat-output-port current-assistant-start current-message-style
         with-cust
         chat completion chat-by-completion
         use-ollama use-llama-cpp current-chat-template)

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

(define current-message-style (make-parameter 'ollama))

(define (build-message role items)
  (match items
    [(list (? hash? h)) h]
    [else
     (define-values (content images) (collect items))
     (cond
       [(eq? (current-message-style) 'ollama)
        (hash-param 'role role
                    'content content
                    'images images)]
       [else
        ;openai style
        (hash-param 'role role
                    'content
                    (if images
                        (cons
                         (hasheq 'type "text" 'text content)
                         (for/list ([img (in-list images)])
                           (define d (string-append "data:image/jpeg;base64," img))
                           (hasheq 'type "image_url" 'image_url (hasheq 'url d))))
                        content))])]))

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
    (chat/history/output (build-message "user" items) output #:assistant-start fake)
    (void)))

(define (completion #:output [output (current-chat-output-port)]
                    . items)
  (define-values (prompt images) (collect items))
  (with-cust _
    (completion/output prompt output)
    (void)))

(define (chat-by-completion
           #:output [output (current-chat-output-port)]
           #:start [fake (current-assistant-start)]
           . items)
    (with-cust _
      (completion/history/output (build-message "user" items)
                                 output
                                 #:assistant-start fake)
      (void)))

(define (use-ollama)
  (local-require "private/main.rkt")
  (current-chat-endpoint ollama-chat-endpoint)
  (current-completion-endpoint ollama-completion-endpoint))

(define (use-llama-cpp)
  (local-require "private/llama-cpp-endpoint.rkt")
  (current-chat-endpoint llama-cpp-chat-endpoint)
  (current-completion-endpoint llama-cpp-completion-endpoint))