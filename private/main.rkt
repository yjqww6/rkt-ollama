#lang racket/base
(require "config.rkt" "history.rkt" "log.rkt"
         racket/match net/http-client json racket/port)
(provide chat generate chat/raw embeddings list-models)

(define (send path data #:method [method "POST"])
  (log-network-trace (network:send data))
  (define-values (status headers chat-port)
    (http-sendrecv
     (current-host) (string-append "/api/" path)
     #:port (current-port) #:method method
     #:data (and data (jsexpr->bytes data))))
  chat-port)

(define (chat/raw messages)
  (define data
    (hash-param
     'model (current-model)
     'messages messages
     'tools (current-tools)
     'stream (box (current-stream))
     'options (current-options)
     'format (current-response-format)))
  (send "chat" data))
  
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
      (with-handlers* ([(λ (e) (and (exn:break? e)
                                    (continuation-prompt-available? break-prompt-tag)))
                        (λ (e)
                          (call/cc
                           (λ (cc)
                             (abort-current-continuation break-prompt-tag cc)))
                          (hasheq 'message (fake-assistant "")))])
        (for/last ([j (in-port read-json chat-port)]
                   #:final (hash-ref j 'done #f))
          (log-network-trace (network:recv j))
          (match j
            [(hash* ['message (hash* ['content content])])
             (write-string content new-output)
             (flush-output new-output)
             j]
            [(hash* ['error err])
             (error 'chat "~a" err)])))
      (close-input-port chat-port)))
  (perf-trace result)
  (current-history
   (append-history
    (current-history)
    message
    (hash-set (hash-ref result 'message)
              'content (get-output-string sp)))))

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
                'context context
                'format (current-response-format)))
  (define chat-port (send "generate" data))
  (define result
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
  (perf-trace result)
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
