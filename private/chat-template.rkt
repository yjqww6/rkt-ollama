#lang racket/base
(require racket/list)
(provide llama3 gemma2)
;;;; mostly for llama.cpp prefill

(define (split-messages messages [merge-system? #t])
  (let*-values ([(sys msgs) (if (string=? (hash-ref (car messages) 'role) "system")
                                (values (car messages) (cdr messages))
                                (values #f messages))]
                [(msgs prefill) (let ([last-message (last msgs)])
                                  (if (string=? (hash-ref last-message 'role) "assistant")
                                      (values (drop-right msgs 1) last-message)
                                      (values msgs #f)))])
    (cond
      [(not merge-system?) (values sys msgs prefill)]
      [sys (values (cons sys msgs) prefill)]
      [else (values msgs prefill)])))

(define (llama3 messages)
  (define s (open-output-string))
  (define-values (msgs prefill) (split-messages messages))
  (for ([msg (in-list msgs)])
    (fprintf s "<|start_header_id|>~a<|end_header_id|>\n\n~a<|eot_id|>"
             (hash-ref msg 'role) (hash-ref msg 'content)))
  (fprintf s "<|start_header_id|>assistant<|end_header_id|>\n\n~a"
           (if prefill
               (hash-ref prefill 'content)
               ""))
  (get-output-string s))

(define (gemma2 messages)
  (define s (open-output-string))
  (define-values (sys msgs prefill) (split-messages messages #f))
  (for/fold ([sys sys])
            ([msg (in-list msgs)]
             [i (in-naturals)])
    (define role
      (let ([r (hash-ref msg 'role)])
        (if (string=? r "assistant") "model" r)))
    (fprintf s "<start_of_turn>~a\n~a<end_of_turn>\n"
             role (if (and sys (string=? role "user"))
                      (string-append (hash-ref sys 'content) "\n\n" (hash-ref msg 'content))
                      (hash-ref msg 'content)))
    (and sys (not (string=? role "user"))))
  (fprintf s "<start_of_turn>model\n~a"
           (if prefill
               (hash-ref prefill 'content)
               ""))
  (get-output-string s))