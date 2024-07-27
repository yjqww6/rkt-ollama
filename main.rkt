#lang racket/base
(require "private/config.rkt" "private/history.rkt" "private/image.rkt"
         (prefix-in p: "private/main.rkt")
         syntax/parse/define
         racket/string racket/match)
(provide (all-from-out "private/config.rkt" "private/history.rkt")
         chat generate last-response undo redo clear)

(define (call-with-cust thunk)
  (let ([cust (make-custodian)])
    (dynamic-wind
     void
     (λ ()
       (parameterize ([current-custodian cust])
         (thunk cust)))
     (λ () (custodian-shutdown-all cust)))))

(define-syntax-parse-rule (with-cust C:id Body:expr ...+)
  (call-with-cust (λ (C) Body ...)))

(define (collect items)
  (let loop ([items items] [strs '()] [imgs '()])
    (cond
      [(null? items) (values (string-join (reverse strs) "\n")
                             (and (not (null? imgs))
                                  (map get-image (reverse imgs))))]
      [(string? (car items)) (loop (cdr items) (cons (car items) strs) imgs)]
      [else (loop (cdr items) strs (cons (car items) imgs))])))

(define (build-message role items)
  (define-values (content images) (collect items))
  (hash-param 'role role
              'content content
              'images images))

(define (chat #:output [output (current-output-port)] #:start [fake #f] . items)
  (with-cust _
    (display-stats
     (p:chat (build-message "user" items) output #:assistant-start fake))))

(define (generate #:output [output (current-output-port)] . items)
  (define-values (prompt images) (collect items))
  (with-cust _
    (display-stats
     (p:generate prompt output #:images images))))

(define (redo #:output [output (current-output-port)] #:start [fake #f])
  (match-define (list history ... user assistant) (current-history))
  (current-history
   (parameterize ([current-history history])
     (display-stats
      (p:chat user output #:assistant-start fake))
     (current-history))))

(define last-response (make-parameter #f))

(define (display-stats j [output (current-output-port)])
  (last-response j)
  (newline output)
  (define (->seconds a)
    (/ a 1e9))
  (when (current-verbose)
    (match j
      [(hash* ['total_duration (app ->seconds total_duration)]
              ['load_duration (app ->seconds load_duration)]
              ['prompt_eval_duration (app ->seconds prompt_eval_duration)]
              ['eval_duration (app ->seconds eval_duration)]
              ['prompt_eval_count prompt_eval_count]
              ['eval_count eval_count])
       
       (fprintf output
                "prompt tokens: ~a~%eval tokens: ~a~%"
                prompt_eval_count eval_count)
       (fprintf output
                "elapsed: ~a~%load: ~a~%prompt: ~a~%response: ~a~%"
                total_duration
                load_duration
                prompt_eval_duration
                eval_duration)
       (fprintf output "prompt ~a token/s~%eval ~a token/s~%"
                (/ prompt_eval_count
                   prompt_eval_duration)
                (/ eval_count
                   eval_duration))]
      [else (void)])))