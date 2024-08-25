#lang racket/base
(require racket/match "config.rkt")
(provide (all-defined-out))

(struct network (data) #:transparent)
(struct network:send network () #:transparent)
(struct network:recv network () #:transparent)

(struct perf (prompt-tokens eval-tokens prompt-duration eval-duration prompt-tokens-per-second eval-tokens-per-second) #:transparent)

(define default-network-trace void)

(define (default-perf-trace p)
  (define (show case tokens duration tokens-per-second)
    (define pad (make-string (string-length case) #\space))
    (cond
      [duration
       (printf "~a: ~a seconds~%" case duration)
       (printf "~a  ~a tokens~%" pad  tokens)
       (printf "~a  ~a token/s~%" pad (or tokens-per-second (/ tokens duration)))]
      [else
       (printf "~a  ~a tokens~%" case tokens)]))
  (when (current-verbose)
    (match p
      [(perf prompt-tokens eval-tokens
             prompt-duration eval-duration
             prompt-tokens-per-second eval-tokens-per-second)
       (newline)
       (show "PROMPT" prompt-tokens prompt-duration prompt-tokens-per-second)
       (show "EVAL" eval-tokens eval-duration eval-tokens-per-second)])))

(define current-network-trace (make-parameter void))
(define current-perf-trace (make-parameter default-perf-trace))

(define (log-network-trace data)
  ((current-network-trace) data))

(define (log-perf-trace data)
  ((current-perf-trace) data))
