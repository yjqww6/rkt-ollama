#lang racket/base
(require "main.rkt"
         racket/list racket/file racket/class racket/match racket/system racket/port
         racket/lazy-require)
(provide (all-defined-out) (all-from-out "main.rkt"))
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
                 [current-chat-output-port #f])
    ((current-chat) "")))

(define default-chat (make-default-chat chat))
(define generate-chat
  (make-default-chat
   (λ (s)
     (when (current-assistant-start)
       (error 'generate-chat "unsupported"))
     (generate s #:use-context? #t))))

(define current-chat (make-parameter default-chat))

(define current-say-command (make-parameter "say"))
(define (say str)
  (match-define (list #f #f pid #f f)
    (process/ports
     (open-output-nowhere) (open-input-string str) (open-output-nowhere)
     (current-say-command)))
  (f 'wait))

(define say-chat
  (λ (s)
    (define-values (in out) (make-pipe))
    (with-cust _
      (define thr
        (thread
         (λ ()
           (for ([line (in-lines in)])
             (say line)))))
      (parameterize ([current-chat-output-port (combine-output (current-chat-output-port) out)])
        (default-chat s)
        (close-output-port out)
        (thread-wait thr)))))

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
     (when (> (string-length prompt) 0)
       (clip prompt))]
    [else (clip content)])
  (current-history history))

(module+ main
  (require expeditor (submod expeditor configure)
           racket/port racket/cmdline racket/runtime-path
           (for-syntax racket/base))
  (define ns (namespace-anchor->namespace here))
  (define-runtime-module-path-index llama-cpp '(submod "main.rkt" llama-cpp))

  (port-count-lines! (current-output-port))
  (current-context-window 8192)
  (command-line
   #:program "rkt-ollama-repl"
   #:once-each
   [("-m" "--model") m "default model" (current-model m)]
   [("-s" "--system") s "system prompt" (current-system s)]
   [("-v" "--verbose") "verbose messages" (current-verbose #t)]
   [("-c" "--context-window") c "context window size" (current-context-window (string->number c))]
   [("--host") h "ollama host" (current-host h)]
   [("--port") p "ollama port" (current-port (string->number p))]
   [("--llama-cpp") tpl
                    "use llama.cpp chatter with template"
                    (namespace-require llama-cpp ns)
                    (case tpl
                      [("oai" "openai") (current-chat (make-default-chat (dynamic-require llama-cpp 'chat)))]
                      [else
                       ((dynamic-require llama-cpp 'current-chat-template)
                        (dynamic-require llama-cpp (string->symbol (string-append "template:" tpl))))
                       (current-chat (make-default-chat (dynamic-require llama-cpp 'chat-by-completion)))])]
   #:multi
   [("-r" "--require") file "required file" (namespace-require file ns)])
  
  (define (command-input? in)
    (regexp-match-peek #px"^\\s*," in))


  ;; expeditor seems buggy, use M^v when paste massive texts
  (define (multi-input? in)
    (regexp-match-peek #px"^\"\"\"" in))
  (define multi-input-end #px"(\r\n|\n|\r)\"\"\"$")

  ;; may reach the limit of type ahead buffer
  (define (multi-line? in)
    (regexp-match-peek #px"^'''" in))

  (define (multi-line-paste? in)
    (regexp-match-peek #px"^```(\r\n|\n|\r|$)" in))
  (define multi-input-paste-end #px"(\r\n|\n|\r)```$")

  (define (forward p in)
    (read-bytes (bytes-length (car p)) in))
  (struct message (content))
  (struct cmd (content))
  (struct lines (first))

  (define uploaded! #f)
  (struct uploaded ())

  (define running? (make-parameter #f))

  (define (read-multi-line first)
    (define o (open-output-string))
    (write-string first o)
    (with-handlers ([exn:break? (λ (_) #f)])
      (let f ()
        (define v (read-line))
        (cond
          [(or (eof-object? v) (string=? v "'''"))
           (get-output-string o)]
          [else
           (newline o)
           (write-string v o)
           (f)]))))

  (define (run thunk)
    (call-with-continuation-prompt
     (λ ()
       (parameterize ([running? #t])
         (call-with-continuation-prompt
          thunk
          break-prompt-tag
          (λ (cc)
            (newline)
            (display "Accept as history[y/n]:")
            (when (regexp-match? #px"^\\s*y" (read-line))
              (cc))))))))
  (define (run-chat s)
    (run (λ () ((current-chat) s))))

  ;; prefixes
  ;; , starts a racket command
  ;; """ starts multi line block in expeditor without triggering shortcuts
  ;; ''' starts multi line block with read-line
  ;;; ^ starts a output prefix
  (define (reader orig)
    (lambda (in)
      (cond
        [uploaded!
         (set! uploaded! #f)
         (uploaded)]
        [(eof-object? (peek-byte in)) eof]
        [(eqv? #\^ (peek-char in))
         (read-char in)
         (current-output-prefix (port->string in))
         (uploaded)]
        [(eqv? #\$ (peek-char in))
         (read-char in)
         (current-system (port->string in))
         (uploaded)]
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
        [(multi-line? in)
         =>
         (λ (p)
           (forward p in)
           (lines (port->string in)))]
        [(multi-line-paste? in)
         =>
         (λ (p)
           (forward p in)
           (current-paste-text (car (regexp-split multi-input-paste-end (port->string in))))
           (uploaded))]
        [else
         (message (port->string in))])))

  (define (ready orig)
    (lambda (in)
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
        [(multi-line? in) #t]
        [else #t])))

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
      (let loop ()
        (define v (expeditor-read
                   ee
                   #:prompt
                   (string-append
                    (if (current-paste-text) "text " "")
                    (if (current-image) "img " "")
                    (if (current-output-prefix) "pre " "")
                    ">>>")))
        (cond
          [(eof-object? v) (expeditor-close ee)]
          [(uploaded? v) (loop)]
          [(lines? v)
           (define s (read-multi-line (lines-first v)))
           (when (string? s)
             (run-chat s))
           (loop)]
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
                 (for ([v (in-list r)])
                   (unless (void? v)
                     (println v)))))))
           (loop)])))
    (void)))
