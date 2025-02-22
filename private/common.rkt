#lang racket/base
(require net/http-client
         racket/port
         racket/string
         "config.rkt"
         "history.rkt"
         "json.rkt"
         "log.rkt")
(provide (all-defined-out))

(struct response (port))

(struct response:event-stream response ()
  #:property prop:sequence
  (λ (resp)
    (define port (response-port resp))
    (in-producer
     (λ ()
       (let loop ()
         (define l (read-line port 'any-one))
         (when (non-empty-string? l)
           (log-network-trace (network:recv l)))
         (cond
           [(eof-object? l) l]
           [(not (non-empty-string? l)) (loop)]
           [(not (string-prefix? l "data: ")) (string->jsexpr l)]
           [(string=? l "data: [DONE]") eof]
           [else (string->jsexpr (substring l 5))])))
     eof-object?)))

(define (close-response resp)
  (cond
    [(response-port resp) => close-input-port]))

(define (send path j #:method [method "POST"] #:return-headers [return-headers #f])
  (define data (and j (djson->bytes j)))
  (when data
    (log-network-trace (network:send data)))
  (define-values (status headers port)
    (http-sendrecv
     (current-host) path
     #:port (current-port) #:method method
     #:headers '("Content-Type: application/json")
     #:data data))
  (cond
    [return-headers (values port headers)]
    [else port]))

(define (call/interrupt proc [on-abort #f])
  (with-handlers* ([(λ (e) (and (exn:break? e)
                                (continuation-prompt-available? break-prompt-tag)))
                    (λ (e)
                      (call/cc
                       (λ (cc)
                         (abort-current-continuation break-prompt-tag cc)))
                      (when on-abort (on-abort)))])
    (proc)))

(define (call/history message proc
                      #:assistant-start [fake #f])
  (define messages
    ((current-messages-preprocessor)
     (append-history
      (make-system (current-system))
      (current-history)
      message
      (fake-assistant fake))))
  (define msg (proc messages))
  (current-history
   (append-history
    (current-history)
    message
    msg))
  msg)

(define current-chat-endpoint
  (make-parameter (λ (messages stream-output) (error 'chat "no endpoint"))))

(define current-completion-endpoint
  (make-parameter (λ (prompt stream-output) (error 'completion "no endpoint"))))

(define (chat/history/output message stream-output
                             #:assistant-start [fake #f])
  (call/history
   message
   (λ (messages)
     (call/interrupt
      (λ ()
        ((current-chat-endpoint) messages stream-output))))
   #:assistant-start fake))

(define (completion/history/output message stream-output
                                   #:assistant-start [fake #f] #:template [template (current-chat-template)])
  (call/history
   message
   (λ (messages)
     (define prompt ((current-template-postprocessor) (template messages)))
     (when fake (stream-output fake))
     (call/interrupt
      (λ ()
        (define content ((current-completion-endpoint) prompt stream-output))
        (hasheq 'role "assistant" 'content (if fake (string-append fake content) content)))))
   #:assistant-start fake))

(define (completion/output prompt stream-output)
  ((current-completion-endpoint) prompt stream-output))

(define current-tokenize-endpoint
  (make-parameter (make-parameter (λ (prompt) (error 'tokenize "no endpoint")))))

(define (tokenize prompt)
  ((current-tokenize-endpoint) prompt))
