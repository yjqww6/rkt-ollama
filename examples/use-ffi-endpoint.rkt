#lang racket/base
(require "../main.rkt" racket/match)
(provide use-ffi-endpoint enlarge-context!)

(module suffix racket/base
  (require racket/match)
  (provide make-suffixes-checker)

  (struct Check
    (pattern n pi [current-state #:mutable]))

  (define (make-suffix-check pattern)
    (define n (bytes-length pattern))
    (define chk (Check pattern n (compute-kmp-restart-vector pattern n) 0))
    (λ (b)
      (check-suffix-step chk b)))
  (define (compute-kmp-restart-vector pattern m)
    (define pi (make-vector m 0))
    (let loop ([i 1] [length 0])
      (cond
        [(>= i m) pi]
        [(= (bytes-ref pattern i)
            (bytes-ref pattern length))
         (vector-set! pi i (add1 length))
         (loop (add1 i) (add1 length))]
        [(zero? length)
         (loop (add1 i) 0)]
        [else
         (loop i (vector-ref pi (sub1 length)))])))
  (define (check-suffix-step check c)
    (match-define (Check pattern n pi state) check)
    (define next-state
      (let loop ([state state])
        (cond
          [(and (< state n)
                (= (bytes-ref pattern state) c))
           (+ state 1)]
          [(zero? state) state]
          [else
           (loop (vector-ref pi (- state 1)))])))
    (set-Check-current-state! check next-state)
    (cond
      [(= next-state n) pattern]
      [(> next-state 0) #t]
      [else #f]))
  (define (make-suffixes-checker bstops)
    (define checkers (map make-suffix-check bstops))
    (λ (b)
      (define ls
        (for/list ([c (in-list checkers)])
          (c b)))
      (or
       (for/first ([item (in-list ls)]
                   #:when (bytes? item))
         item)
       (for/or ([item (in-list ls)])
         item)))))
(require 'suffix)

(define (make-stop-flusher output stops)
  (define bstops (map string->bytes/utf-8 stops))
  (define suffiex-checker (make-suffixes-checker bstops))
  (define buf (make-bytes 256))
  (define pos 0)
  (lambda (bstr n)
    (let/ec break
      (define partial?
        (for/fold ([partial? #f])
                  ([i (in-range n)])
          (define b (bytes-ref bstr i))
          (bytes-set! buf (+ pos i) b)
          (match (suffiex-checker b)
            [(? bytes? suffix)
             (write-bytes buf output 0 (- (+ pos i 1) (bytes-length suffix)))
             (flush-output output)
             (set! pos 0)
             (log-resp-trace (hasheq 'stop_type "word"))
             (break #t)]
            [p (or p partial?)])))
      (set! pos (+ pos n))
      (cond
        [(= n 0)
         (write-bytes buf output 0 pos)
         (flush-output output)
         #t]
        [partial?
         #f]
        [else
         (write-bytes buf output 0 pos)
         (flush-output output)
         (set! pos 0)
         #f]))))

(define current-model-context (make-parameter #f))

(define ((ffi-endpoint completion mc) prompt output)
  (define stop (current-stop))
  (completion mc
              prompt (cond
                       [(or (not stop) (null? stop)) output]
                       [else (make-stop-flusher output stop)])
              #:n-predict (current-num-predict)
              #:perf (λ (p pp ppt tg tgt)
                       (log-perf-trace (perf p tg (/ ppt 1000) (/ tgt 1000)
                                             (/ pp (/ ppt 1000)) (/ tg (/ tgt 1000)))))
              #:grammar (current-grammar)
              #:progress (and (current-verbose)
                              (let ([t (current-inexact-monotonic-milliseconds)])
                                (λ (off n kind)
                                  (printf "~a\t/~a\t ~a\t~ams~%"
                                          off n kind (- (current-inexact-monotonic-milliseconds) t)))))))

(define (use-ffi-endpoint model ctx #:kvcache-quant? [kvcache-quant? #f] #:template [tpl #f])
  (define init-model! (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'init-model!))
  (define completion (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'completion))
  (define old-mc (current-model-context))
  (when old-mc
    (define free (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'free-model-context))
    (free old-mc)
    (current-model-context #f))
  (define mc (init-model! #:path model #:context ctx #:kvcache-quant? kvcache-quant?))
  (current-model-context mc)
  (current-completion-endpoint (ffi-endpoint completion mc))
  (when tpl
    (define f (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'ffi-template-postprocessor))
    (current-template-postprocessor (λ (parts) (f mc parts)))))

(define (enlarge-context! ctx #:kvcache-quant? [kvcache-quant? #f])
  (define f (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'enlarge-context!))
  (f (current-model-context) ctx #:kvcache-quant? kvcache-quant?))