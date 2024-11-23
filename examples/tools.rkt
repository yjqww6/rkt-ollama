#lang racket/base
(require racket/system net/uri-codec "../tool.rkt")
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
