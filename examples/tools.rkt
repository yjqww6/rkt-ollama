#lang racket/base
(require racket/system racket/date racket/exn "../tool.rkt")
(provide (all-defined-out))

(define (system/string cmd)
  (define o (open-output-string))
  (parameterize ([current-output-port o]
                 [current-error-port o])
    (system cmd)
    (get-output-string o)))

(define-tool (shell_exec [cmd : string #:desc "command line to be executed"])
  #:desc (format "execute shell command. system type: ~a" (system-type 'os))
  (system/string cmd))

(define example-tools (list shell_exec))

(define-syntax-rule (begin-tool Body ...)
  (with-handlers* ([exn:fail? (λ (e) (hasheq 'error (exn->string e)))])
    Body ...))

(define-tool (list_dir [path : string #:desc "path of the directory to be listed"]
                       [pattern : string #:def #f #:desc "optional regexp patterns"])
  #:desc "list contents of a directory"
  (begin-tool
    (define reg (and pattern (pregexp pattern)))
    (define dir (expand-user-path path))
    (parameterize ([date-display-format 'iso-8601])
      (for/list ([p (in-list (directory-list dir))]
                 #:when (or (not reg) (regexp-match? pattern p))
                 #:do [(define cp (build-path dir p))
                       (define type (file-or-directory-type cp))]
                 #:when type)
        (define mtime (date->string (seconds->date (file-or-directory-modify-seconds cp))))
        (define h (hasheq 'name (path->string p)
                          'mtime mtime))
        (cond
          [(eq? type 'file)
           (hash-set h 'size (file-size cp))]
          [else
           (hash-set h 'type (symbol->string type))])))))

(define-tool (read_file [path : string #:desc "path of the file to be read"]
                        [offset : integer #:def #f #:desc "optional file offset"]
                        [limit : integer #:def 1000 #:desc "max size to be read, default to 1000"])
  #:desc "read content of a text file"
  (begin-tool
    (call-with-input-file* (expand-user-path path)
      (λ (p)
        (when offset
          (file-position p offset))
        (define str (read-string limit p))
        (define next (file-position p))
        (if (eof-object? (peek-char p))
            (hasheq 'content str 'all #t)
            (hasheq 'content str 'next_offset next 'all #f))))))

(define filesystem-tools (list list_dir read_file))
