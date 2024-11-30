#lang racket/base
(require "config.rkt" "history.rkt" "log.rkt" "common.rkt"
         racket/match racket/port json)
(provide  generate ollama-chat-endpoint ollama-completion-endpoint
          embeddings list-models)

(define (build-options)
  (hash-param
   'num_ctx (current-context-window)
   'temperature (current-temperature)
   'repeat_penalty (current-repeat-penalty)
   'repeat_last_n (current-repeat-last-n)
   'top_p (current-top-p)
   'min_p (current-min-p)
   'top_k (current-top-k)
   'seed (current-seed)
   'num_predict (current-num-predict)
   'stop (current-stop)
   (current-options)))

(define (chat messages)
  (define data
    (hash-param
     'model (current-model)
     'messages messages
     'tools (current-tools)
     'stream (box (current-stream))
     'options (build-options)
     'format (and (current-enforce-json) "json")))
  (response (send "/api/chat" data)))

(define (handle-chat-response resp output)
  (for ([j resp]
        #:final (hash-ref j 'done #f))
    (log-resp-trace j)
    (perf-trace j)
    (match j
      [(hash* ['message (hash* ['content content])])
       (write-string content output)
       (flush-output output)]
      [(hash* ['error err])
       (error 'chat/history "~a" err)])))

(define (ollama-chat-endpoint messages output [fake #f])
  (define resp (chat messages))
  (when fake (write-string fake output))
  (handle-chat-response resp output)
  (close-response resp))

(define (handle-generate-response resp output)
  (for ([j resp])
    (log-resp-trace j)
    (perf-trace j)
    (match j
      [(hash* ['done done] ['response content])
       (write-string content output)
       (flush-output output)]
      [(hash* ['error err])
       (error 'generate "~a" err)])))

(define (generate prompt
                  #:images [images #f]
                  #:template [template #f]
                  #:raw? [raw #f]
                  #:context [context #f])
  (define data
    (hash-param 'model (current-model)
                'prompt prompt
                'system (default-system)
                'images images
                'options (current-options)
                'stream (box (current-stream))
                'raw raw
                'template template
                'context context
                'format (and (current-enforce-json) "json")))
  (response (send "/api/generate" data)))

(define (ollama-completion-endpoint prompt output)
  (define resp (generate prompt #:raw? #t))
  (handle-generate-response resp output)
  (close-response resp))

(define (perf-trace j)
  (match j
    [(hash* ['total_duration total_duration]
            ['load_duration load_duration]
            ['prompt_eval_duration prompt_eval_duration]
            ['eval_duration eval_duration]
            ['prompt_eval_count prompt_eval_count]
            ['eval_count eval_count])
     (log-perf-trace (perf prompt_eval_count eval_count (/ prompt_eval_duration 1e9) (/ eval_duration 1e9) #f #f))]
    [else (void)]))

(define (embeddings prompt)
  (define data
    (hash-param
     'model (current-model)
     'input prompt))
  (define chat-port (send "/api/embed" data))
  (define j (begin0 (read-json chat-port) (close-input-port chat-port)))
  (log-network-trace (network:recv j))
  (hash-ref j 'embeddings))

(define (list-models [detailed? #f])
  (define chat-port (send "/api/tags" #f #:method "GET"))
  (define j (begin0 (read-json chat-port) (close-input-port chat-port)))
  (log-network-trace (network:recv j))
  (define models (hash-ref j 'models))
  (cond
    [detailed? models]
    [else (for/list ([m (in-list models)])
            (hash-ref m 'model))]))

