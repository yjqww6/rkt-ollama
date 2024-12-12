#lang racket/base
(require racket/date racket/string racket/match racket/control json racket/port
         "../tool.rkt" "../repl.rkt" "tool-template.rkt")
(provide (all-from-out "../tool.rkt") (all-defined-out))

;; example tools
(define-tool (get_current_time) #:desc "get the current time"
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))

(define-tool (nop) #:desc "do nothing. call this tool if you don't need to call other tools"
  #t)

(define default-tools (list get_current_time))
(define current-execute (make-parameter #f))
(define (tool-repl-prompt)
  (string-append "TOOL:" (default-repl-prompt)))

(define (execute)
  (match (current-history)
    [(list _ ... (and assistant (hash* ['role "assistant"])))
     ((current-execute) assistant)]
    [else (void)]))

(define (call/last-response proc)
  (match (current-history)
    [(list _ ... (hash* ['role "assistant"] ['content content]))
     (proc content)]
    [else (void)]))

(define ((make-auto-execute-chat [chat (current-chat)]) s)
  (chat s)
  (match (current-history)
    [(list _ ... (and assistant (hash* ['role "assistant"])))
     (unless (let-values ([(line col pos) (port-next-location (current-output-port))])
               (eq? col 0))
       (newline))
     ((current-execute) assistant #:auto? #t)]
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

(define current-tool-role (make-parameter "user"))

(define current-auto-guard (make-parameter #f))

(define (make-exec parse callback make-response #:parse-content? [parse-content? #t])
  (λ (assistant #:auto? [auto? #f])
    (let/ec k
      (define calls (parse (if parse-content? (hash-ref assistant 'content) assistant)))
      (when (and calls (not (null? calls)))
        (when auto?
          (define guard (current-auto-guard))
          (when (and guard (not (guard calls)))
            (k)))
        (define resp
          (string-join
           (for/list ([call (in-list calls)])
             (make-response (callback call)))
           "\n"))
        ((current-chat) (hasheq 'role (current-tool-role) 'content resp))))))

(define (make-json-gbnf #:defs [defs #f] . root)
  (define root-string
    (let ([s (open-output-string)])
      (for ([r (in-list root)])
        (fprintf s "~s " r))
      (get-output-string s)))
  (string-append
   "root ::= " root-string "\n"
   #<<GBNF
value  ::= object | array | string | number | ("true" | "false" | "null") ws

object ::=
  "{" ws (
            string ":" ws value
    ("," ws string ":" ws value)*
  )? "}" ws

array  ::=
  "[" ws (
            value
    ("," ws value)*
  )? "]" ws

string ::=
  "\"" (
    [^"\\\x7F\x00-\x1F] |
    "\\" (["\\bfnrt] | "u" [0-9a-fA-F]{4}) # escapes
  )* "\"" ws

number ::= ("-"? ([0-9] | [1-9] [0-9]{0,15})) ("." [0-9]+)? ([eE] [-+]? [0-9] [1-9]{0,15})? ws

# Optional space: by convention, applied in this grammar after literal chars when allowed
ws ::= | " " | "\n" [ \t]{0,20}

GBNF
   (or defs "")))

(define (make-constrained-completion-endpoint trigger grammar)
  (define has-tool (make-parameter #f))
  (define old-trace (current-resp-trace))
  (define (new-trace net)
    (match net
      [(or (hash* ['stop_type "word"])
           (hash* ['stopping_word (? (λ (s) (string=? s trigger)))]))
       (has-tool #t)]
      [else (void)])
    (old-trace net))

  (define old-completion (current-completion-endpoint))
  (λ (prompt output)
    (define p (open-output-string))
    (parameterize ([has-tool #f])
      (parameterize ([current-stop (list trigger)]
                     [current-resp-trace new-trace])
        (old-completion prompt (combine-output output p)))
      (when (has-tool)
        (define new-prompt (string-append prompt (get-output-string p) trigger))
        (parameterize ([current-grammar grammar])
          (write-string trigger output)
          (old-completion new-prompt output))))))

(define (use-nous-tools #:tools [tools default-tools]
                        #:auto? [auto? #f]
                        #:constrained? [constrained? (and (current-chat-template) #t)])
  (reset
   (when constrained?
     (define new-completion
       (make-constrained-completion-endpoint "<tool_call>" (make-json-gbnf 'ws 'object "</tool_call>")))
     (shift k (parameterize ([current-completion-endpoint new-completion]) (k))))
  
   (define callback (tools-callback tools))
   (define exec
     (make-exec parse-nous-toolcall callback make-nous-response))
   (parameterize ([current-messages-preprocessor (make-system-preprocessor (λ (sys) (make-nous-system-template tools sys)))]
                  [current-execute exec]
                  [current-repl-prompt tool-repl-prompt])
     (reset
      (when auto?
        (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
      (repl)))))

(define (make-always-chat tools parse call/setup)
  (define old-chat (current-chat))
  (define new-tools (if (memq nop tools) tools (cons nop tools)))
  (λ (s)
    (match s
      [(hash* ['role r]) #:when (string=? (current-tool-role) r) (old-chat s)]
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
    (make-exec parse-mistral-toolcall callback make-mistral-response))
  (parameterize ([current-execute exec]
                 [current-temperature 0.2]
                 [current-repl-prompt tool-repl-prompt]
                 [current-tool-role "tool"])
    (reset
     (cond
       [always?
        (define old-completion (current-completion-endpoint))
        (define always-chat
          (make-always-chat
           tools parse-mistral-toolcall
           (λ (new-tools proc)
             (define tools-string (tools->string new-tools))
             (parameterize ([current-tools-string tools-string]
                            [current-output-prefix "[TOOL_CALLS]"]
                            [current-grammar (make-json-gbnf 'ws "[" 'object "]")])
               (proc)))))
        (shift k (parameterize ([current-chat always-chat]) (k)))]
       [else
        (define tools-string (tools->string tools))
        (shift k (parameterize ([current-tools-string tools-string])
                   (k)))])
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
     (repl))))

(define (use-llama3-tools #:tools [tools default-tools]
                          #:auto? [auto? #f]
                          #:always? [always? #t])
  (define callback (tools-callback tools))
  (define exec
    (make-exec parse-llama3-toolcall callback jsexpr->string))
  (parameterize ([current-execute exec]
                 [current-messages-preprocessor
                  (make-system-preprocessor (λ (sys) (make-llama3-system-template sys)))]
                 [current-repl-prompt tool-repl-prompt]
                 [current-tool-role "ipython"])
    (reset
     (cond
       [always?
        (define always-chat
          (make-always-chat
           tools parse-llama3-toolcall
           (λ (new-tools proc)
             (parameterize ([current-messages-preprocessor
                             (make-last-user-preprocessor (λ (user) (make-llama3-prompt new-tools user)))]
                            [current-enforce-json #t])
               (proc)))))
        (shift k (parameterize ([current-chat always-chat]) (k)))]
       [else
        (shift k (parameterize ([current-messages-preprocessor
                                 (make-last-user-preprocessor (λ (user) (make-llama3-prompt tools user)))])
                   (k)))])
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
     (repl))))

(define (use-llama-tools #:tools [tools default-tools]
                         #:auto? [auto? #f]
                         #:constrained? [constrained? (and (current-chat-template) #t)])
  (reset
   (when constrained?
     (define new-completion
       (make-constrained-completion-endpoint
        "<function="
        (make-json-gbnf 'name ">" 'ws 'object 'ws "</function>"
                        #:defs "name ::= [A-Za-z_]+")))
     (shift k (parameterize ([current-completion-endpoint new-completion]) (k))))
   (define callback (tools-callback tools))
   (define exec
     (make-exec parse-llama-toolcall callback jsexpr->string))
   (parameterize ([current-messages-preprocessor (make-system-preprocessor (λ (sys) (make-llama-system-template tools sys)))]
                  [current-execute exec]
                  [current-repl-prompt tool-repl-prompt]
                  [current-tool-role "ipython"])
     (reset
      (when auto?
        (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
      (repl)))))

(define (use-ollama-tools #:tools [tools default-tools]
                          #:auto? [auto? #f])
  (define callback (tools-callback tools))
  (define exec
    (make-exec (λ (assistant)
                 (match assistant
                   [(hash* ['tool-calls (list (hash 'function calls) ...)])
                    calls]
                   [else #f]))
               callback
               jsexpr->string
               #:parse-content? #f))
  (parameterize ([current-tools (map tool-desc tools)]
                 [current-repl-prompt tool-repl-prompt]
                 [current-execute exec]
                 [current-tool-role "tool"])
    (reset
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat)]) (k))))
     (repl))))
