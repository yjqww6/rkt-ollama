#lang racket/base
(require "main.rkt" racket/file racket/lazy-require)
(define-namespace-anchor here)
(define current-chat (make-parameter chat))

(define current-image (make-parameter #f))
(lazy-require [racket/gui/base (get-file)])
(define (upload-image)
  (define p (get-file))
  (when p
    (current-image (file->bytes p))))

(module+ main
  (require expeditor (submod expeditor configure)
           racket/port racket/cmdline)

  (define no-preload #f)
  (command-line
   #:program "rkt-ollama-repl"
   #:once-each
   [("-m" "--model") m "default model" (current-model m)]
   [("-s" "--system") s "system prompt" (current-system s)]
   [("-v" "--verbose") "verbose messages" (current-verbose #t)]
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


  ;; expeditor seems buggy, use ''' when pasting massive texts
  (define (multi-input? in)
    (regexp-match-peek #px"^\"\"\"" in))
  (define multi-input-end #px"(\r\n|\n|\r)\"\"\"$")

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
         ((current-chat)
          (cond
            [(current-image)
             =>
             (λ (img)
               (current-image #f)
               (list s img))]
            [else s]))))))

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
    
    (parameterize ([current-namespace ns]
                   [current-expeditor-reader
                    (reader (current-expeditor-reader))]
                   [current-expeditor-ready-checker
                    (ready (current-expeditor-ready-checker))])
      (let loop ()
        (define v (expeditor-read
                   ee
                   #:prompt (if (current-image) "img>>>" ">>>")))
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
