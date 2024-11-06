#lang racket/base
(require racket/date racket/string racket/match "tool.rkt" "repl.rkt"
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
  
