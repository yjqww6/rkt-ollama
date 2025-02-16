#lang racket/base
(require net/http-client
         racket/port
         "config.rkt"
         "history.rkt"
         "json.rkt"
         "log.rkt")
(provide (all-defined-out))

(struct response (port))

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
  (define sp (open-output-string))
  (define tool-calls '())
  (define (save-tool-calls ts)
    (set! tool-calls (append tool-calls ts)))
  (proc messages sp save-tool-calls)
  (current-history
   (append-history
    (current-history)
    message
    (hash-param 'role "assistant"
                'content (get-output-string sp)
                'tool_calls (and (not (null? tool-calls))
                                 tool-calls)))))

(define current-chat-endpoint
  (make-parameter (λ (messages output [fake #f]) (error 'chat "no endpoint"))))

(define current-completion-endpoint
  (make-parameter (λ (prompt output) (error 'completion "no endpoint"))))

(define (chat/history/output message output
                             #:assistant-start [fake #f])
  (call/history
   message
   (λ (messages sp tool-calls-output)
     (call/interrupt
      (λ ()
        ((current-chat-endpoint) messages (combine-output sp output) fake tool-calls-output))))
   #:assistant-start fake))

(define (completion/history/output message output
                                   #:assistant-start [fake #f] #:template [template (current-chat-template)])
  (call/history
   message
   (λ (messages sp _)
     (define prompt ((current-template-postprocessor) (template messages)))
     (define new-output (combine-output sp output))
     (when fake (write-string fake new-output))
     (call/interrupt
      (λ ()
        ((current-completion-endpoint) prompt new-output))))
   #:assistant-start fake))

(define (completion/output prompt output)
  ((current-completion-endpoint) prompt output))

(define current-tokenize-endpoint
  (make-parameter (make-parameter (λ (prompt) (error 'tokenize "no endpoint")))))

(define (tokenize prompt)
  ((current-tokenize-endpoint) prompt))
