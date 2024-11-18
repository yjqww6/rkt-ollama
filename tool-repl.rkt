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

(define (make-exec parse callback make-response role)
  (λ (content)
    (define calls (parse content))
    (when (and calls (not (null? calls)))
      (define resp
        (string-join
         (for/list ([call (in-list calls)])
           (make-response (callback call)))
         "\n"))
      ((current-chat) (hasheq 'role role 'content resp)))))

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
   (define exec
     (make-exec parse-nous-toolcall callback make-nous-response "user"))
   (parameterize ([current-messages-preprocessor (make-system-preprocessor (λ (sys) (make-nous-system-template tools sys)))]
                  [current-execute exec]
                  [current-repl-prompt tool-repl-prompt])
     (reset
      (when auto?
        (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
      (repl)))))

(define (make-always-chat tools role parse call/setup)
  (define old-chat (current-chat))
  (define new-tools (if (memq nop tools) tools (cons nop tools)))
  (λ (s)
    (match s
      [(hash* ['role r]) #:when (string=? role r) (old-chat s)]
      [else
       (call/setup new-tools (λ () (old-chat s)))
       (call/last-response
        (λ (content)
          (match (parse content)
            [(cons (hash* ['name "nop"]) _)
             (undo)
             (old-chat s)]
            [else (void)])))])))

;;; chat-by-completion is needed
(define (use-mistral-tools #:tools [tools default-tools]
                           #:auto? [auto? #f]
                           #:always? [always? #t])
  (define callback (tools-callback tools))
  (define exec
    (make-exec parse-mistral-toolcall callback make-mistral-response "tool"))
  (parameterize ([current-execute exec]
                 [current-temperature 0.2]
                 [current-repl-prompt tool-repl-prompt])
    (reset
     (cond
       [always?
        (define always-chat
          (make-always-chat
           tools "tool" parse-mistral-toolcall
           (λ (new-tools proc)
             (define tools-string (tools->string new-tools))
             (parameterize ([current-chat-template (λ (messages) (mistral messages #:tools tools-string))]
                            [current-output-prefix " ["]
                            ; TODO grammar
                            )
               (proc)))))
        (shift k (parameterize ([current-chat always-chat]) (k)))]
       [else
        (define tools-string (tools->string tools))
        (shift k (parameterize ([current-chat-template (λ (messages) (mistral messages #:tools tools-string))])
                   (k)))])
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
     (repl))))

(define (use-llama3-tools #:tools [tools default-tools]
                          #:auto? [auto? #f]
                          #:always? [always? #t])
  (define callback (tools-callback tools))
  (define exec
    (make-exec parse-llama3-toolcall callback jsexpr->string "ipython"))
  (parameterize ([current-execute exec]
                 [current-messages-preprocessor
                  (make-system-preprocessor (λ (sys) (make-llama3-system-template sys)))]
                 [current-repl-prompt tool-repl-prompt])
    (reset
     (cond
       [always?
        (define always-chat
          (make-always-chat
           tools "ipython" parse-llama3-toolcall
           (λ (new-tools proc)
             (parameterize ([current-messages-preprocessor
                             (make-last-user-preprocessor (λ (user) (make-llama3-prompt new-tools user)))]
                            [current-json-schema (hasheq)])
               (proc)))))
        (shift k (parameterize ([current-chat always-chat]) (k)))]
       [else
        (shift k (parameterize ([current-messages-preprocessor
                                 (make-last-user-preprocessor (λ (user) (make-llama3-prompt tools user)))])
                   (k)))])
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
     (repl))))
