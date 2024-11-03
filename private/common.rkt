#lang racket/base
(require racket/match json net/http-client "config.rkt" "log.rkt" "history.rkt")
(provide (all-defined-out))

(struct response (port)
  #:property prop:sequence
  (λ (resp)
    (in-port
     (λ (p)
       (define l (read-line p))
       (cond
         [(eof-object? l) l]
         [else
          (log-network-trace (network:recv l))
          (string->jsexpr l)]))
     (response-port resp))))

(struct response/producer response (proc)
  #:property prop:sequence
  (λ (resp)
    (in-producer (response/producer-proc resp) eof-object?)))

(define (close-response resp)
  (cond
    [(response-port resp) => close-input-port]))

(define (send path j #:method [method "POST"])
  (define data (and j (jsexpr->bytes j)))
  (when data
    (log-network-trace (network:send data)))
  (define-values (status headers port)
    (http-sendrecv
     (current-host) path
     #:port (current-port) #:method method
     #:headers '("Content-Type: application/json")
     #:data data))
  port)

(define (call/interrupt proc [on-abort #f])
  (with-handlers* ([(λ (e) (and (exn:break? e)
                                (continuation-prompt-available? break-prompt-tag)))
                    (λ (e)
                      (call/cc
                       (λ (cc)
                         (abort-current-continuation break-prompt-tag cc)))
                      (when on-abort (on-abort)))])
    (proc)))

(define (call/history message proc #:assistant-start [fake #f])
  (define messages
    (append-history
     (make-system (current-system))
     (current-history)
     message
     (fake-assistant fake)))
  (define sp (open-output-string))
  (when fake
    (write-string fake sp))
  (proc messages sp)
  (current-history
   (append-history
    (current-history)
    message
    (hasheq 'role "assistant"
            'content (get-output-string sp)))))
