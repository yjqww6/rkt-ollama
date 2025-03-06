#lang racket/base
(require racket/class
         racket/file
         racket/lazy-require
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         "main.rkt"
         "private/chat-template.rkt")
(provide (all-defined-out) (all-from-out "main.rkt" "private/chat-template.rkt"))
(define-namespace-anchor here)

(define current-image (make-parameter #f))
(define current-paste-text (make-parameter #f))
(define current-paste-text-as-prefix? (make-parameter #t))

(lazy-require [racket/gui/base (get-file)])
(define (upload-image)
  (define p (get-file))
  (when p
    (current-image (file->bytes p))))

(define clip
  (let ([c #f])
    (define (init)
      (unless c
        (set! c (dynamic-require 'racket/gui/base 'the-clipboard)))
      (current-milliseconds))
    (case-lambda
      [()
       (define t (init))
       (or (send c get-clipboard-bitmap t)
           (send c get-clipboard-string t))]
      [(s)
       (define t (init))
       (send c set-clipboard-string s t)])))

(define (paste [append? #f])
  (define pasted (clip))
  (cond
    [(string? pasted)
     (cond
       [(not append?)
        (current-paste-text pasted)]
       [(current-paste-text)
        =>
        (λ (s) (current-paste-text (string-append s "\n" pasted)))]
       [else (current-paste-text pasted)])]
    [pasted (current-image pasted)]
    [else (void)]))

(define (default-make-prompt prompt #:paste-text paste-text)
  (cond
    [(not paste-text) prompt]
    [else
     (define p (list "```" paste-text "```"))
     (if (current-paste-text-as-prefix?)
         (list p prompt)
         (list prompt p))]))

(define current-make-prompt (make-parameter default-make-prompt))

(define ((make-default-chat chat) s)
  (cond
    [(hash? s) (chat s)]
    [else
     (parameterize ([current-assistant-start (current-output-prefix)])
       (chat (list ((current-make-prompt) s #:paste-text (current-paste-text))
                   (current-image)))
       (newline))
     (current-output-prefix #f)
     (current-paste-text #f)
     (current-image #f)]))

(define (warmup)
  (parameterize ([current-assistant-start (current-assistant-start)]
                 [current-paste-text (current-paste-text)]
                 [current-image (current-image)]
                 [current-num-predict 0]
                 [current-output-port (open-output-nowhere)]
                 [current-history (current-history)]
                 [current-chat-output-port #f])
    ((current-chat) "")))

(define current-chat (make-parameter #f))
(define current-repl (make-parameter #f))
(define (repl)
  ((current-repl)))

(define current-say-command (make-parameter "say"))
(define (say str)
  (match-define (list #f #f pid #f f)
    (process/ports
     (open-output-nowhere) (open-input-string str) (open-output-nowhere)
     (current-say-command)))
  (f 'wait))

(define ((make-say-chat chat) s)
  (define-values (in out) (make-pipe))
  (with-cust _
    (define thr
      (thread (λ ()
                (for ([line (in-lines in)])
                  (say line)))))
    (parameterize ([current-chat-output-port (combine-output (current-chat-output-port)
                                                             out)])
      (chat s)
      (close-output-port out)
      (thread-wait thr))))

(define current-output-prefix (make-parameter #f))

(define (redo)
  (match-define (list history ... user assistant) (current-history))
  (current-history
   (parameterize ([current-history history])
     ((current-chat) user)
     (current-history))))

(define (continue [prefill #f])
  (match-define (list history ... user (hash* ['role "assistant"] ['content fake]))
    (current-history))
  (current-history
   (parameterize ([current-history history])
     (parameterize ([current-assistant-start
                     (if prefill (string-append fake prefill) fake)])
       ((current-chat) user))
     (current-history))))

(define (take-last-prompt)
  (match-define (list history ... (hash* ['role "user"] ['content content]) assistant)
    (current-history))
  (match (regexp-match #px"^```\n(.*)\n```(.*)$" content)
    [(list _ paste prompt)
     (current-paste-text paste)
     (when (positive? (string-length prompt))
       (clip prompt))]
    [else (clip content)])
  (current-history history))

(define (last-response)
  (match-define (hash 'content content #:open) (last (current-history)))
  content)

(define (default-repl-prompt)
  (string-append
   (if (current-paste-text) "text " "")
   (if (current-image) "img " "")
   (if (current-output-prefix) "pre " "")
   ">>>"))
(define current-repl-prompt (make-parameter default-repl-prompt))

(define (use-llama-cpp-endpoint tpl)
  (default-endpoint (cons "localhost" 8080))
  (current-message-style #f)
  (case tpl
    [("oai" "openai") (current-chat (make-default-chat chat))]
    [else
     (current-chat-template (chat-template tpl))
     (current-chat (make-default-chat chat-by-completion))])
  (use-llama-cpp))

(module+ main
  (require expeditor (submod expeditor configure)
           racket/port racket/cmdline racket/runtime-path
           (for-syntax racket/base))
  (define-runtime-module-path-index repl '(submod ".."))

  (define ns (namespace-anchor->empty-namespace here))
  (namespace-require 'racket ns)
  (namespace-require repl ns)

  (port-count-lines! (current-output-port))
  (current-context-window 8192)
  (command-line
   #:program "rkt-ollama-repl"
   #:once-each
   [("-m" "--model") m "default model" (current-model m)]
   [("-s" "--system") s "system prompt" (default-system s)]
   [("-v" "--verbose") "verbose messages" (current-verbose #t)]
   [("-c" "--context-window") c "context window size" (current-context-window (string->number c))]
   [("--host") h "ollama host" (current-host h)]
   [("--port") p "ollama port" (current-port (string->number p))]
   [("--llama-cpp") tpl
                    "use llama.cpp chatter with template"
                    (use-llama-cpp-endpoint tpl)]
   #:multi
   [("-r" "--require") file "required file" (namespace-require file ns)]
   [("-e" "--expr") expr "expression" (eval (read (open-input-string expr)) ns)])

  (unless (current-chat)
    (use-ollama)
    (current-chat (make-default-chat chat)))

  (cond
    [(default-system) => current-system])
  
  (define (command-input? in)
    (regexp-match-peek #px"^\\s*," in))


  ;; expeditor seems buggy, use M^v when paste massive texts
  (define (multi-input? in)
    (regexp-match-peek #px"^\"\"\"" in))
  (define multi-input-end #px"(\r\n|\n|\r)\"\"\"$")

  (define (multi-line-paste? in)
    (regexp-match-peek #px"^```(\r\n|\n|\r|$)" in))
  (define multi-input-paste-end #px"(\r\n|\n|\r)```$")

  (define (forward p in)
    (read-bytes (bytes-length (car p)) in))
  (struct message (content))
  (struct cmd (content))

  (define uploaded! #f)
  (struct refreshing ())

  (define running? (make-parameter #f))

  (define (run thunk)
    (parameterize-break
     #f
     (call-with-continuation-prompt
      (λ ()
        (parameterize ([running? #t])
          (parameterize-break
           #t
           (call-with-continuation-prompt
            thunk
            break-prompt-tag
            (λ (cc)
              (newline)
              (display "Accept as history[y/n]:")
              (when (regexp-match? #px"^\\s*y" (read-line (current-input-port) 'any))
                (cc))))))))))
  (define (run-chat s)
    (run (λ () ((current-chat) s))))

  ;; prefixes
  ;; , starts a racket command
  ;; """ starts multi line block in expeditor without triggering shortcuts
  ;;; ^ starts a output prefix
  (define ((reader orig) in)
    (cond
      [uploaded!
       (set! uploaded! #f)
       (refreshing)]
      [(eof-object? (peek-byte in)) eof]
      [(eqv? #\^ (peek-char in))
       (read-char in)
       (current-output-prefix (port->string in))
       (refreshing)]
      [(eqv? #\$ (peek-char in))
       (read-char in)
       (current-system (port->string in))
       (refreshing)]
      [(eqv? #\! (peek-char in))
       (read-char in)
       (cmd (port->string in))]
      [(eqv? #\/ (peek-char in))
       (read-char in)
       (port->list read in)]
      [(command-input? in)
       =>
       (λ (p)
         (forward p in)
         (orig in))]
      [(multi-input? in)
       =>
       (λ (p)
         (forward p in)
         (message (car (regexp-split multi-input-end (port->string in)))))]
      [(multi-line-paste? in)
       =>
       (λ (p)
         (forward p in)
         (current-paste-text (car (regexp-split multi-input-paste-end (port->string in))))
         (refreshing))]
      [else (message (port->string in))]))

  (define ((ready orig) in)
    (cond
      [(command-input? in)
       =>
       (λ (p)
         (forward p in)
         (orig in))]
      [(multi-input? in)
       =>
       (λ (p)
         (forward p in)
         (regexp-match? multi-input-end in))]
      [(multi-line-paste? in)
       =>
       (λ (p)
         (forward p in)
         (regexp-match? multi-input-paste-end in))]
      [else #t]))

  (when (terminal-port? (current-input-port))
    (expeditor-configure)
    (define ee (expeditor-open '()))
    (current-expeditor-color-enabled #f)
    (expeditor-bind-key!
     "^C"
     (λ (ee entry c)
       (cond
         [(running?)
          (ee-reset-entry ee entry c)
          (break-thread (current-thread))]
         [else
          (ee-reset-entry ee entry c)])))
    (expeditor-bind-key!
     "\\ei"
     (λ (ee entry c)
       (upload-image)
       (set! uploaded! #t)
       #f))
    (expeditor-bind-key!
     "\\ev"
     (λ (ee entry c)
       (paste)
       (set! uploaded! #t)
       #f))
    
    (parameterize ([current-namespace ns]
                   [current-expeditor-reader
                    (reader (current-expeditor-reader))]
                   [current-expeditor-ready-checker
                    (ready (current-expeditor-ready-checker))])
      (define (loop)
        (define v (parameterize ([running? #f])
                    (expeditor-read ee #:prompt ((current-repl-prompt)))))
        (cond
          [(eof-object? v) (void)]
          [(refreshing? v) (loop)]
          [(message? v)
           (run-chat (message-content v))
           (loop)]
          [(cmd? v)
           (run (λ () (system (cmd-content v))))
           (loop)]
          [else
           (call-with-continuation-prompt
            (λ ()
              (call-with-values
               (λ () (run (λ () (eval v ns))))
               (λ r
                 (for ([v (in-list r)]
                       #:unless (void? v))
                   (println v))))))
           (loop)]))
      (current-repl loop)
      (loop))
    (void (expeditor-close ee))))
