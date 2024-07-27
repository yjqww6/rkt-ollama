#lang racket/base
(require "main.rkt")
(define-namespace-anchor here)
(define current-chat (make-parameter chat))

(module+ main
  (require expeditor racket/port racket/cmdline)

  (command-line
   #:program "rkt-ollama-repl"
   #:once-each
   [("-m" "--model") m "default model" (current-model m)]
   [("-s" "--system") s "system prompt" (current-system s)]
   [("-v" "--verbose") "verbose messages" (current-verbose #t)]
   [("--host") h "ollama host" (current-host h)]
   [("--port") p "ollama port" (current-port p)])
  
  (define (command-input? in)
    (regexp-match-peek #px"^\\s*," in))
  (define (multi-input? in)
    (regexp-match-peek #px"^\"\"\"" in))
  (define (multi-end? str)
    (regexp-match #px"\"\"\"$" str))
  (struct message (content))

  (when (terminal-port? (current-input-port))
    (define ee (expeditor-open '()))
    (expeditor-configure)
    (current-expeditor-color-enabled #f)
    (define ns (namespace-anchor->namespace here))
    
    (parameterize ([current-namespace ns]
                   [current-expeditor-reader
                    (let ([orig (current-expeditor-reader)])
                      (lambda (in)
                        (cond
                          [(command-input? in)
                           =>
                           (λ (p)
                             (read-bytes (bytes-length (car p)) in)
                             (orig in))]
                          [(eof-object? (peek-byte in)) eof]
                          [(multi-input? in)
                           =>
                           (λ (p)
                             (read-bytes (bytes-length (car p)) in)
                             (define str (port->string in))
                             (cond
                               [(multi-end? str)
                                =>
                                (λ (p)
                                  (message
                                   (substring
                                    str 0
                                    (- (string-length str)
                                       (string-length (car p))))))]
                               [else (message str)]))]
                          [else
                           (message (port->string in))])))]
                   [current-expeditor-ready-checker
                    (let ([orig (current-expeditor-ready-checker)])
                      (lambda (in)
                        (cond
                          [(command-input? in)
                           =>
                           (λ (p)
                             (read-bytes (bytes-length (car p)) in)
                             (orig in))]
                          [(multi-input? in)
                           =>
                           (λ (p)
                             (read-bytes (bytes-length (car p)) in)
                             (let loop ()
                               (define v (read-line in))
                               (cond
                                 [(eof-object? v) #f]
                                 [(multi-end? v) #t]
                                 [else (loop)])))]
                          [else #t])))])
      (let loop ()
        (define v (expeditor-read ee #:prompt ">>>"))
        (cond
          [(eof-object? v) (expeditor-close ee)]
          [(message? v)
           (call-with-continuation-prompt
            (λ ()
              ((current-chat) (message-content v))))
           (loop)]
          [else
           (call-with-continuation-prompt
            (λ ()
              (call-with-values
               (λ () (eval v ns))
               (λ r
                 (for ([v (in-list r)])
                   (unless (void? v)
                     (println v)))))))
           (loop)])))
    (void)))
