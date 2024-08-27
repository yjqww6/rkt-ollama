#lang racket/base
(require racket/list)
(provide llama3)
;;;; mostly for llama.cpp prefill

(define (llama3 messages)
  (define s (open-output-string))
  (define last-message (last messages))
  (define prefill? (string=? (hash-ref last-message 'role) "assistant"))
  (for ([msg (in-list (if prefill? (drop-right messages 1) messages))])
    (fprintf s "<|start_header_id|>~a<|end_header_id|>\n\n~a<|eot_id|>"
             (hash-ref msg 'role) (hash-ref msg 'content)))
  (fprintf s "<|start_header_id|>assistant<|end_header_id|>\n\n~a"
           (if prefill?
               (hash-ref last-message 'content)
               ""))
  (get-output-string s))