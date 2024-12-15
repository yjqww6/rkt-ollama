#lang racket/base
(require db
         json
         racket/date
         racket/exn
         racket/file
         racket/match
         racket/system
         "../tool.rkt")
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
        (define v (read-string limit p))
        (define str (if (eof-object? v) "" v))
        (define next (file-position p))
        (if (eof-object? (peek-char p))
            (hasheq 'content str 'all #t)
            (hasheq 'content str 'next_offset next 'all #f))))))

(define-tool (create_file [path : string #:desc "path of the file to be created"]
                          [content : string #:desc "content of the creating file"])
  #:desc "create a new file"
  (begin-tool
    (with-handlers* ([exn:fail:filesystem:exists?
                      (λ (e) (hasheq 'err "file exists, use a different name instead."))])
      (display-to-file content (expand-user-path path))
      #t)))

(define-tool (create_dir [path : string #:desc "path of the directory to be created"])
  #:desc "create a new directory"
  (begin-tool
    (make-directory* (expand-user-path path))
    #t))

(define filesystem-tools (list list_dir read_file create_file create_dir))

(define (default-db)
  (sqlite3-connect #:database "example.db"
                   #:mode 'create
                   #:use-place #t))

(define current-db (make-parameter #f))

(define (get-db)
  (cond
    [(current-db) => values]
    [else
     (define db (default-db))
     (current-db db)
     db]))

(define-tool (db_query [stmt : string #:desc "sql stmt to be executed"]
                       [args : array #:desc "arguments if stmt need prepared" #:def '()
                             #:items (hasheq 'anyOf (list (hasheq 'type "string") (hasheq 'type "number")))])
  #:desc "execute a sql stmt in current sqlite database. remember to check the table info before performing a query or insert"
  (begin-tool
    (define db (get-db))
    (define res (apply query db (prepare db stmt)
                       (for/list ([arg (in-list args)])
                         (if (eq? (json-null) arg) sql-null arg))))
    (match res
      [(simple-result (list (cons ks vs) ...))
       (for/hasheq ([k (in-list ks)]
                    [v (in-list vs)]
                    #:when (jsexpr? v))
         (values k v))]
      [(rows-result headers rows)
       (define names
         (for/list ([h (in-list headers)])
           (string->symbol (cdr (assq 'name h)))))
       (for/list ([row (in-list rows)])
         (for/hasheq ([n (in-list names)]
                      [f (in-vector row)])
           (values
            n
            (cond
              [(jsexpr? f) f]
              [(sql-null? f) (json-null)]
              [else (error 'db_query "unsupported type")]))))])))
