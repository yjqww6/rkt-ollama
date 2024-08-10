#lang racket/base
(require "config.rkt" "history.rkt" "log.rkt"
         racket/match racket/generator net/http-client json)
(provide  generate chat response? close-response
          chat/history/output chat/history generate/output
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

(struct response/producer response (proc)
  #:property prop:sequence
  (λ (resp)
    (in-producer (response/producer-proc resp) eof-object?)))

(define (close-response resp)
  (cond
    [(response-port resp) => close-input-port]))

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

(define (chat-with-history message proc #:before [before #f] #:after [after #f]
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
  (when before (before resp))
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
  (when after (after resp))
  (current-history
   (append-history
    (current-history)
    message
    (hash-set (hash-ref result 'message)
              'content (get-output-string sp)))))

(define (chat/history/output message output
                             #:assistant-start [fake #f])
  (chat-with-history
   message
   #:before (and fake (λ (resp) (write-string fake output)))
   (λ (content json)
     (write-string content output)
     (flush-output output))
   #:after close-response
   #:assistant-start fake))

(define (chat/history message
                      #:assistant-start [fake #f]
                      #:json? [json? #f]
                      #:close-response? [close-response? #t])
  (define prompt (make-continuation-prompt-tag))
  (call-with-continuation-prompt
   (λ ()
     (chat-with-history
      message
      #:before
      (λ (resp)
        (call-with-composable-continuation
         (λ (cc)
           (abort-current-continuation
            prompt
            (λ () (response/producer
                   (response-port resp)
                   (generator () (cc))))))))
      (λ (content json)
        (yield (if json? json content)))
      #:after (and close-response? close-response)
      #:assistant-start fake)
     (yield eof))
   prompt))

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
