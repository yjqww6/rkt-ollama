#lang racket/base
(require "config.rkt" "history.rkt" "log.rkt"
         racket/match net/http-client json)
(provide  generate chat response? close-response
          chat/history chat/output generate/output
          embeddings list-models)

(struct response (port)
  #:property prop:sequence
  (λ (resp)
    (in-port
     (λ (p) (define j (read-json p))
       (log-network-trace (network:recv j))
       (perf-trace j)
       j)
     (response-port resp))))

(define (close-response resp)
  (close-input-port (response-port resp)))

(define (send path data #:method [method "POST"])
  (log-network-trace (network:send data))
  (define-values (status headers chat-port)
    (http-sendrecv
     (current-host) (string-append "/api/" path)
     #:port (current-port) #:method method
     #:data (and data (jsexpr->bytes data))))
  chat-port)

(define (chat messages)
  (define data
    (hash-param
     'model (current-model)
     'messages messages
     'tools (current-tools)
     'stream (box (current-stream))
     'options (current-options)
     'format (current-response-format)))
  (response (send "chat" data)))

(define (chat/history message proc #:before [before #f] #:after [after #f]
                      #:assistant-start [fake #f])
  (define messages
    (append-history 
     (make-system (current-system))
     (current-history)
     message
     (fake-assistant fake)))
  (define sp (open-output-string))
  (define resp (chat messages))
  (when fake
    (write-string fake sp))
  (when before (before))
  (define result
    (with-handlers* ([(λ (e) (and (exn:break? e)
                                  (continuation-prompt-available? break-prompt-tag)))
                      (λ (e)
                        (call/cc
                         (λ (cc)
                           (abort-current-continuation break-prompt-tag cc)))
                        (hasheq 'message (fake-assistant "")))])
      (for/last ([j resp]
                 #:final (hash-ref j 'done #f))
        (match j
          [(hash* ['message (hash* ['content content])])
           (write-string content sp)
           (proc content j)
           j]
          [(hash* ['error err])
           (error 'chat/history "~a" err)]))))
  (close-response resp)
  (when after (after))
  (current-history
   (append-history
    (current-history)
    message
    (hash-set (hash-ref result 'message)
              'content (get-output-string sp)))))

(define (chat/output message output
                     #:assistant-start [fake #f])
  (chat/history
   message
   (λ (content json)
     (write-string content output)
     (flush-output output))
   #:before (and fake (λ () (write-string fake output)))
   #:assistant-start fake))

(define (generate prompt
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
                'context context
                'format (current-response-format)))
  (response (send "generate" data)))

(define (generate/output prompt output
                         #:images [images #f]
                         #:template [template #f]
                         #:stream? [stream? #t]
                         #:raw? [raw #f]
                         #:context [context #f])
  (define resp (generate prompt
                         #:images images
                         #:template template
                         #:stream? stream?
                         #:raw? raw
                         #:context context))
  (define result
    (for/last ([j resp])
      (match j
        [(hash* ['done done] ['response content])
         (write-string content output)
         (flush-output output)
         j]
        [(hash* ['error err])
         (error 'generate "~a" err)])))
  (close-response resp)
  result)

(define (perf-trace j)
  (match j
    [(hash* ['total_duration total_duration]
            ['load_duration load_duration]
            ['prompt_eval_duration prompt_eval_duration]
            ['eval_duration eval_duration]
            ['prompt_eval_count prompt_eval_count]
            ['eval_count eval_count])
     (log-perf-trace (perf prompt_eval_count eval_count prompt_eval_duration eval_duration))]
    [else (void)]))

(define (embeddings prompt)
  (define data
    (hash-param
     'model (current-model)
     'input prompt))
  (define chat-port (send "embed" data))
  (define j (begin0 (read-json chat-port) (close-input-port chat-port)))
  (log-network-trace (network:recv j))
  (hash-ref j 'embeddings))

(define (list-models [detailed? #f])
  (define chat-port (send "tags" #f #:method "GET"))
  (define j (begin0 (read-json chat-port) (close-input-port chat-port)))
  (log-network-trace (network:recv j))
  (define models (hash-ref j 'models))
  (cond
    [detailed? models]
    [else (for/list ([m (in-list models)])
            (hash-ref m 'model))]))
