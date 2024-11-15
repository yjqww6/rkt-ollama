#lang racket/base
(require racket/date racket/string racket/match json
         "tool.rkt" "repl.rkt" "private/chat-template.rkt" "main.rkt"
         (submod "tool.rkt" template))
(provide (all-from-out "tool.rkt") (all-defined-out))

;; example tools
(define-tool (get_current_time) #:desc "get the current time"
  (date->string (current-date) #t))

(define default-tools (list get_current_time))
(define current-execute (make-parameter #f))

(define (execute)
  (match (current-history)
    [(list _ ... (hash* ['role "assistant"] ['content content]))
     ((current-execute) content)]
    [else (void)]))

(define ((make-auto-execute-chat [chat (current-chat)]) s)
  (chat s)
  (match (current-history)
    [(list _ ... (hash* ['role "assistant"] ['content content]))
     ((current-execute) content)]
    [else (void)]))

(define (use-nous-tools #:tools [tools default-tools]
                        #:system [system (current-system)]
                        #:history [history '()]
                        #:auto? [auto #t])
  (define callback (tools-callback tools))
  (define (exec content)
    (define calls (parse-nous-toolcall content))
    (when (and calls (not (null? calls)))
      (define resp
        (string-join
         (for/list ([call (in-list calls)])
           (make-nous-response (callback call)))
         "\n"))
      ((current-chat) (make-user resp))))
  (parameterize ([current-system (make-nous-system-template tools system)]
                 [current-history history]
                 [current-execute exec]
                 [current-repl-prompt (λ () (string-append "TOOL:" (default-repl-prompt)))])
    (if auto
        (parameterize ([current-chat (make-auto-execute-chat)])
          (repl))
        (repl))))

;; need chat-by-completion & llama.cpp
(define (use-nous-tools/constrained
         #:tools [tools default-tools]
         #:system [system (current-system)]
         #:history [history '()]
         #:auto? [auto #t])
  (local-require "private/common.rkt" racket/port racket/string json)
  (define has-tool (make-parameter #f))
  (define old-trace (current-resp-trace))
  (define (new-trace net)
    (match net
      [(hash* ['stopping_word "<tool_call>"])
       (has-tool #t)]
      [else (void)])
    (old-trace net))

  (define old-completion (current-completion-endpoint))
  (define (new-completion prompt output)
    (define p (open-output-string))
    (parameterize ([has-tool #f])
      (parameterize ([current-stop '("<tool_call>")]
                     [current-resp-trace new-trace])
        (old-completion prompt (combine-output output p)))
      (when (has-tool)
        (define new-prompt (string-append prompt (get-output-string p) "<tool_call> "))
        (parameterize ([current-json-schema (hasheq)])
          (write-string "<tool_call> " output)
          (old-completion new-prompt output)
          (write-string " </tool_call>" output)))))
  (parameterize ([current-completion-endpoint new-completion])
    (use-nous-tools #:tools tools #:system system #:history history #:auto? auto)))

;;; chat-by-completion is needed
(define (use-mistral-tools #:tools [tools default-tools]
                           #:system [system (current-system)]
                           #:history [history '()]
                           #:auto? [auto #t])
  (define callback (tools-callback tools))
  (define (exec content)
    (define calls (parse-mistral-toolcall content))
    (when (and calls (not (null? calls)))
      (define resp
        (string-join
         (for/list ([call (in-list calls)])
           (make-mistral-response (callback call)))
         "\n"))
      ((current-chat) (hasheq 'role "tool" 'content resp))))
  (define tools-string (tools->string tools))
  (parameterize ([current-history history]
                 [current-execute exec]
                 [current-temperature 0.2]
                 [current-repl-prompt (λ () (string-append "TOOL:" (default-repl-prompt)))]
                 [current-chat-template (λ (messages) (mistral messages #:tools tools-string))])
    (if auto
        (parameterize ([current-chat (make-auto-execute-chat)])
          (repl))
        (repl))))

(define (use-nemotron-tools #:tools [tools default-tools]
                            #:system [system (current-system)]
                            #:history [history '()]
                            #:auto? [auto #t])
  (define callback (tools-callback tools))
  (define (exec content)
    (define call (parse-nemotron-toolcall content))
    (define resp (make-nemotron-response (callback call)))
    (when resp
      ((current-chat) (hasheq 'role "ipython" 'content resp))))
  (parameterize ([current-system (make-nemotron-system-template tools system)]
                 [current-history history]
                 [current-execute exec]
                 [current-repl-prompt (λ () (string-append "TOOL:" (default-repl-prompt)))])
    (if auto
        (parameterize ([current-chat (make-auto-execute-chat)])
          (repl))
        (repl))))
