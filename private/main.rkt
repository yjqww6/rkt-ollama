#lang racket/base
(require "config.rkt" "history.rkt"
         racket/match net/http-client json racket/port)
(provide chat generate chat/raw)

(define (post path data)
  (define-values (status headers chat-port)
    (http-sendrecv
     (current-host) (string-append "/api/" path)
     #:port (current-port) #:method "POST"
     #:data (jsexpr->bytes data)))
  chat-port)

(define (chat/raw messages)
  (define data
    (hash-param
     'model (current-model)
     'messages messages
     'tools (current-tools)
     'stream (box (current-stream))
     'options (current-options)))
  (post "chat" data))
  
(define (chat message output
              #:assistant-start [fake #f])
  (define messages
    (append-history 
     (make-system (current-system))
     (current-history)
     message
     (fake-assistant fake)))

  (define sp (open-output-string))
  (define new-output (combine-output output sp))
  (define chat-port (chat/raw messages))
  (when fake
    (write-string fake new-output)
    (flush-output new-output))
  
  (define result
    (begin0
      (for/or ([j (in-port read-json chat-port)])
        (match j
          [(hash* ['done done] ['message (hash* ['content content])])
           (write-string content new-output)
           (flush-output new-output)
           (and done j)]
          [(hash* ['error err])
           (error 'chat "~a" err)]))
      (close-input-port chat-port)))

  (current-history
   (append-history
    (current-history)
    message
    (hash-set (hash-ref result 'message)
              'content (get-output-string sp))))
  result)

(define (generate prompt output
                  #:images [images #f]
                  #:template [template #f]
                  #:stream? [stream? #t]
                  #:raw? [raw #f]
                  #:context [context #f])
  (define data
    (hash-param 'model (current-model)
                'prompt prompt
                'system (current-system)
                'images images
                'options (current-options)
                'stream (box (current-stream))
                'raw raw
                'template template
                'context context))
  (define chat-port (post "generate" data))
  (begin0
    (for/or ([j (in-port read-json chat-port)])
      (match j
        [(hash* ['done done] ['response content])
         (write-string content output)
         (flush-output output)
         (and done j)]
        [(hash* ['error err])
         (error 'generate "~a" err)]))
    (close-input-port chat-port)))
