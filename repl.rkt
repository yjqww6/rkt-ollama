#lang racket/base
(require "main.rkt" racket/file racket/class racket/lazy-require)
(define-namespace-anchor here)
(define current-chat (make-parameter chat))

(define current-image (make-parameter #f))
(define current-paste-text (make-parameter #f))
(define paste-text-as-prefix? (make-parameter #t))

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

(module+ main
  (require expeditor (submod expeditor configure)
           racket/port racket/cmdline)

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
   [("--no-preload") "don't ask to preload the model on startup" (set! no-preload #t)])
  (define preload-evt
    (cond
      [no-preload
       always-evt]
      [else (thread preload)]))
  
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

  (define ns (namespace-anchor->namespace here))
  (define (run s)
    (sync preload-evt)
    (call-with-continuation-prompt
     (λ ()
       (parameterize ([running? #t])
         (define (take c)
           (begin0 (c) (c #f)))
         ((current-chat)
          (list (and (paste-text-as-prefix?) (take current-paste-text))
                s
                (and (not (paste-text-as-prefix?)) (take current-paste-text))
                (take current-image)))))))

  (define (reader orig)
    (lambda (in)
      (cond
        [uploaded!
         (set! uploaded! #f)
         (uploaded)]
        [(eof-object? (peek-byte in)) eof]
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
          (ee-reset-entry/break ee entry c)])))
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
                    ">>>")))
        (cond
          [(eof-object? v) (expeditor-close ee)]
          [(uploaded? v) (loop)]
          [(lines? v)
           (define s (read-multi-line (lines-first v)))
           (when (string? s)
             (run s))
           (loop)]
          [(message? v)
           (run (message-content v))
           (loop)]
          [else
           (call-with-continuation-prompt
            (λ ()
              (call-with-values
               (λ () (parameterize ([running? #t]) (eval v ns)))
               (λ r
                 (for ([v (in-list r)])
                   (unless (void? v)
                     (println v)))))))
           (loop)])))
    (void)))
