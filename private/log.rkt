#lang racket/base
(require racket/match "config.rkt")
(provide (all-defined-out))

(struct network (data) #:transparent)
(struct network:send network () #:transparent)
(struct network:recv network () #:transparent)

(struct perf (prompt-tokens eval-tokens prompt-duration eval-duration) #:transparent)

(define default-network-trace void)

(define (default-perf-trace p)
  (define (->secs a)
    (and (number? a) (/ a 1e9)))
  (define (show case tokens duration)
    (define pad (make-string (string-length case) #\space))
    (cond
      [duration
       (printf "~a: ~a seconds~%" case duration)
       (printf "~a  ~a tokens~%" pad  tokens)
       (printf "~a  ~a token/s~%" pad (/ tokens duration))]
      [else
       (printf "~a  ~a tokens~%" case tokens)]))
  (when (current-verbose)
    (match p
      [(perf prompt-tokens eval-tokens
             (app ->secs prompt-duration) (app ->secs eval-duration))
       (newline)
       (show "PROMPT" prompt-tokens prompt-duration)
       (show "EVAL" eval-tokens eval-duration)])))

(define current-network-trace (make-parameter void))
(define current-perf-trace (make-parameter default-perf-trace))

(define (log-network-trace data)
  ((current-network-trace) data))

(define (log-perf-trace data)
  ((current-perf-trace) data))
