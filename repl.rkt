#lang racket/base
(require "main.rkt" racket/file racket/class racket/match racket/system racket/port
         racket/lazy-require)
(define-namespace-anchor here)

(define current-image (make-parameter #f))
(define current-paste-text (make-parameter #f))
(define current-paste-text-as-prefix? (make-parameter #t))

(lazy-require [racket/gui/base (get-file)])
(define (upload-image)
  (define p (get-file))
  (when p
    (current-image (file->bytes p))))

(define (paste)
  (define t (current-milliseconds))
  (define c (dynamic-require 'racket/gui/base 'the-clipboard))
  (cond
    [(send c get-clipboard-bitmap t)
     => current-image]
    [(send c get-clipboard-string t)
     => current-paste-text]))

(define (default-make-prompt prompt #:paste-text paste-text)
  (cond
    [(not paste-text) prompt]
    [else
     (define p (list "```" paste-text "```"))
     (if (current-paste-text-as-prefix?)
         (list p prompt)
         (list prompt p))]))

(define current-make-prompt (make-parameter default-make-prompt))

(define preload-evt (box always-evt))

(define (default-chat s)
  (define (take c)
    (begin0 (c) (c #f)))
  (sync (unbox preload-evt))
  (parameterize ([current-assistant-start (take current-output-prefix)])
    (chat (list ((current-make-prompt) s #:paste-text (take current-paste-text))
                (take current-image)))
    (newline)))

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

(module+ main
  (require expeditor (submod expeditor configure)
           racket/port racket/cmdline)
  (define ns (namespace-anchor->namespace here))

  (port-count-lines! (current-output-port))
  (define no-preload #f)
  (current-context-window 8192)
  (command-line
   #:program "rkt-ollama-repl"
   #:once-each
   [("-m" "--model") m "default model" (current-model m)]
   [("-s" "--system") s "system prompt" (current-system s)]
   [("-v" "--verbose") "verbose messages" (current-verbose #t)]
   [("-c" "--context-window") c "context window size" (current-context-window (string->number c))]
   [("--host") h "ollama host" (current-host h)]
   [("--port") p "ollama port" (current-port p)]
   [("--no-preload") "don't ask to preload the model on startup" (set! no-preload #t)]
   #:multi
   [("-r" "--require") file "required file" (namespace-require file ns)])

  (unless no-preload
    (set-box! preload-evt
              (handle-evt (thread preload)
                          (λ (v) (set-box! preload-evt always-evt)))))
  
  (define (command-input? in)
    (regexp-match-peek #px"^\\s*," in))


  ;; expeditor seems buggy, use M^v when paste massive texts
  (define (multi-input? in)
    (regexp-match-peek #px"^\"\"\"" in))
  (define multi-input-end #px"(\r\n|\n|\r)\"\"\"$")

  ;; may reach the limit of type ahead buffer
  (define (multi-line? in)
    (regexp-match-peek #px"^'''" in))

  (define (forward p in)
    (read-bytes (bytes-length (car p)) in))
  (struct message (content))
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
