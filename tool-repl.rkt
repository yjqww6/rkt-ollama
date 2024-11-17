#lang racket/base
(require racket/date racket/string racket/match racket/control json
         "tool.rkt" "repl.rkt" "private/chat-template.rkt" "private/history.rkt" "main.rkt"
         (submod "tool.rkt" template))
(provide (all-from-out "tool.rkt") (all-defined-out))

;; example tools
(define-tool (get_current_time) #:desc "get the current time"
  (date->string (current-date) #t))

(define-tool (nop) #:desc "do nothing. call this tool if you don't need to call other tools"
  #t)

(define default-tools (list get_current_time))
(define current-execute (make-parameter #f))
(define (tool-repl-prompt)
  (string-append "TOOL:" (default-repl-prompt)))

(define (execute)
  (match (current-history)
    [(list _ ... (hash* ['role "assistant"] ['content content]))
     ((current-execute) content)]
    [else (void)]))

(define (call/last-response proc)
  (match (current-history)
    [(list _ ... (hash* ['role "assistant"] ['content content]))
     (proc content)]
    [else (void)]))

(define ((make-auto-execute-chat [chat (current-chat)]) s)
  (chat s)
  (match (current-history)
    [(list _ ... (hash* ['role "assistant"] ['content content]))
     ((current-execute) content)]
    [else (void)]))

(define ((make-system-preprocessor f [next (current-messages-preprocessor)]) messages)
  (next
   (match messages
     [(cons (hash 'role "system" 'content content) m)
      (cons (hasheq 'role "system" 'content (f content)) m)]
     [else (cons (hasheq 'role "system" 'content (f #f)) messages)])))

(define ((make-last-user-preprocessor f [next (current-messages-preprocessor)]) messages)
  (next
   (match messages
     [(list m ... (hash 'role "user" 'content content))
      (append m (list (hasheq 'role "user" 'content (f content))))]
     [(list m ... (hash 'role "user" 'content content) (and pre (hash* ['role "assistant"])))
      (append m (list (hasheq 'role "user" 'content (f content)) pre))]
     [else messages])))

(define (use-nous-tools #:tools [tools default-tools]
                        #:auto? [auto? #f]
                        #:constrained? [constrained? (and (current-chat-template) #t)])
  (reset
   (when constrained?
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
     (shift k (parameterize ([current-completion-endpoint new-completion]) (k))))
  
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
   (parameterize ([current-messages-preprocessor (make-system-preprocessor (λ (sys) (make-nous-system-template tools sys)))]
                  [current-execute exec]
                  [current-repl-prompt tool-repl-prompt])
     (reset
      (when auto?
        (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
      (repl)))))

;;; chat-by-completion is needed
(define (use-mistral-tools #:tools [tools default-tools]
                           #:auto? [auto? #f]
                           #:always? [always? #t])
  (define new-tools (if (memq nop tools) tools (cons nop tools)))
  (define callback (tools-callback new-tools))
  (define (exec content)
    (define calls (parse-mistral-toolcall content))
    (when (and calls (not (null? calls)))
      (define resp
        (string-join
         (for/list ([call (in-list calls)])
           (make-mistral-response (callback call)))
         "\n"))
      ((current-chat) (hasheq 'role "tool" 'content resp))))
  (define tools-string (tools->string new-tools))
  (parameterize ([current-execute exec]
                 [current-temperature 0.2]
                 [current-repl-prompt tool-repl-prompt])
    (reset
     (when always?
       (define old-chat (current-chat))
       (define (always-chat s)
         (match s
           [(hash* ['role "tool"]) (old-chat s)]
           [else
            (parameterize ([current-chat-template (λ (messages) (mistral messages #:tools tools-string))]
                           [current-output-prefix " ["]
                           ; TODO grammar
                           )
              (old-chat s))
            (unless auto?
              (call/last-response
               (λ (content)
                 (match (parse-mistral-toolcall content)
                   [(list (hash* ['name "nop"]))
                    (undo)
                    (old-chat s)]
                   [else (void)]))))]))
       (shift k (parameterize ([current-chat always-chat]) (k))))
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
     (repl))))

(define (use-llama3-tools #:tools [tools default-tools]
                          #:auto? [auto? #f]
                          #:always? [always? #t])
  (define callback (tools-callback tools))
  (define (exec content)
    (define call (parse-llama3-toolcall content))
    (when call
      (define resp
        (jsexpr->string (callback call)))
      ((current-chat) (hasheq 'role "ipython" 'content resp))))
  (parameterize ([current-execute exec]
                 [current-messages-preprocessor
                  (make-system-preprocessor (λ (sys) (make-llama3-system-template sys)))]
                 [current-repl-prompt tool-repl-prompt])
    (reset
     (cond
       [always?
        (define old-chat (current-chat))
        (define new-tools (if (memq nop tools) tools (cons nop tools)))
        (define (always-chat s)
          (match s
            [(hash* ['role "ipython"]) (old-chat s)]
            [else
             (parameterize ([current-messages-preprocessor
                             (make-last-user-preprocessor (λ (user) (make-llama3-prompt new-tools user)))]
                            [current-json-schema (hasheq)])
               (old-chat s))
             (call/last-response
              (λ (content)
                (match (parse-llama3-toolcall content)
                  [(hash* ['name "nop"])
                   (undo)
                   (old-chat s)]
                  [else (void)])))]))
        (shift k (parameterize ([current-chat always-chat]) (k)))]
       [else
        (shift k (parameterize ([current-messages-preprocessor
                                 (make-last-user-preprocessor (λ (user) (make-llama3-prompt tools user)))])
                   (k)))])
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
     (repl))))
