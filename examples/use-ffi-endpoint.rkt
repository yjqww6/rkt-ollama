#lang racket/base
(require "../main.rkt")
(provide use-ffi-endpoint)

(define (bytes-suffix? buf n sub)
  (and
   (>= n (bytes-length sub))
   (for/and ([b1 (in-bytes buf (- n (bytes-length sub)) n)]
             [b2 (in-bytes sub)])
     (= b1 b2))))

(define (bytes-contains? buf pre-pos n sub)
  (cond
    [(< n (bytes-length sub)) #f]
    [else
     (define lstart (max 0 (- pre-pos (- (bytes-length sub) 1))))
     (define rstart (+ (- n (bytes-length sub)) 1))
     (for/or ([i (in-range lstart rstart)])
       (if (for/and ([b1 (in-bytes buf i)]
                     [b2 (in-bytes sub)])
             (= b1 b2))
           i
           #f))]))

(define (make-stop-flusher output stops)
  (define bstops (map string->bytes/utf-8 stops))
  (define partial-bstops
    (for*/list ([bstop (in-list bstops)]
                [i (in-range 1 (bytes-length bstop))])
      (subbytes bstop 0 i)))
  (define buf (make-bytes 256))
  (define pos 0)
  (lambda (bstr n)
    (for ([i (in-range n)])
      (bytes-set! buf (+ pos i) (bytes-ref bstr i)))
    (set! pos (+ pos n))
    (cond
      [(= n 0)
       (write-bytes buf output 0 pos)
       (flush-output output)
       #t]
      [(for/or ([bstop (in-list bstops)])
         (bytes-contains? buf (- pos n) pos bstop))
       =>
       (λ (i)
         (write-bytes buf output 0 i)
         (flush-output output)
         (set! pos 0)
         (log-resp-trace (hasheq 'stop_type "word"))
         #t)]
      [(for/first ([bstop (in-list partial-bstops)]
                   #:when (bytes-suffix? buf pos bstop))
         bstop)
       #f]
      [else
       (write-bytes buf output 0 pos)
       (flush-output output)
       (set! pos 0)
       #f])))

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
    (define f (dynamic-require 'rkt-ollama/examples/ffi-endpoint 'another-template))
    (current-chat-template (λ (msgs) (f tpl mc msgs)))))