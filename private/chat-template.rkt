#lang racket/base
(require racket/list racket/match racket/sequence racket/string)
(provide chatml llama3 gemma2 minitron minitron/stop mistral mistral/new
         current-tools-string)
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

(define (in-messages msgs #:user [user #f] #:assistant [assistant #f])
  (sequence-map
   (λ (msg)
     (define role (hash-ref msg 'role))
     (values (cond
               [(string=? role "user") (or user role)]
               [(string=? role "assistant") (or assistant role)]
               [else role])
             (string-trim
              (match (hash-ref msg 'content)
                [(? string? c) c]
                [(list (and item (hash* ['type t])) ...)
                 (apply string-append
                        (for/list ([item (in-list item)]
                                   [t (in-list t)]
                                   #:when (string=? t "text"))
                          (hash-ref item 'text)))])
              #:left? #f)))
   msgs))

(define (prefill-content msg)
  (if msg (hash-ref msg 'content) ""))

(define (inject-system-to-first-user msgs system [inject #f])
  (define (default-inject sys usr)
    (string-append sys "\n\n" usr))
  (cond
    [(not system) msgs]
    [else
     (let loop ([msgs msgs])
       (cond
         [(null? msgs) '()]
         [(string=? (hash-ref (car msgs) 'role) "user")
          (cons
           (hash-set (car msgs) 'content
                     ((or inject default-inject) (hash-ref system 'content)
                                                 (hash-ref (car msgs) 'content)))
           (cdr msgs))]
         [else (cons (car msgs) (loop (cdr msgs)))]))]))

(define (chatml messages)
  (define s (open-output-string))
  (define-values (msgs prefill) (split-messages messages))
  (for ([(role content) (in-messages msgs)])
    (fprintf s "<|im_start|>~a\n~a<|im_end|>\n" role content))
  (fprintf s "<|im_start|>assistant\n~a" (prefill-content prefill))
  (get-output-string s))

(define (llama3 messages)
  (define s (open-output-string))
  (define-values (msgs prefill) (split-messages messages))
  (for ([(role content) (in-messages msgs)])
    (fprintf s "<|start_header_id|>~a<|end_header_id|>\n\n~a<|eot_id|>" role content))
  (fprintf s "<|start_header_id|>assistant<|end_header_id|>\n\n~a" (prefill-content prefill))
  (get-output-string s))

(define (gemma2 messages)
  (define s (open-output-string))
  (define-values (sys msgs prefill) (split-messages messages #f))
  (for ([(role content) (in-messages (inject-system-to-first-user msgs sys) #:assistant "model")])
    (fprintf s "<start_of_turn>~a\n~a<end_of_turn>\n"
             role content))
  (fprintf s "<start_of_turn>model\n~a" (prefill-content prefill))
  (get-output-string s))

(define (minitron messages)
  (define s (open-output-string))
  (define-values (sys msgs prefill) (split-messages messages #f))
  (when sys
    (fprintf s "<extra_id_0>System~%~a~%~%" (hash-ref sys 'content)))
  (for ([(role content) (in-messages msgs #:user "User" #:assistant "Assistant")])
    (fprintf s "<extra_id_1>~a~%~a~%" role content))
  (fprintf s "<extra_id_1>Assistant~%~a" (prefill-content prefill))
  (get-output-string s))

(define minitron/stop '("<extra_id_1>"))

(define (last-msg msgs)
  (if (null? msgs) #f (last msgs)))

(define current-tools-string (make-parameter #f))

(define (mistral messages)
  (define s (open-output-string))
  (define-values (sys msgs prefill) (split-messages messages #f))
  (define new-msgs (inject-system-to-first-user msgs sys))
  (define last (last-msg new-msgs))
  (for ([(role content) (in-messages new-msgs)]
        [msg (in-list new-msgs)])
    (cond
      [(string=? role "assistant") (fprintf s "~a</s>" content)]
      [(string=? role "tool") (fprintf s "~a" content)]
      [else
       (when (eq? last msg)
         (define tools (current-tools-string))
         (when tools
           (fprintf s "[AVAILABLE_TOOLS] ~a[/AVAILABLE_TOOLS]" tools)))
       (fprintf s "[INST] ~a[/INST]" content)]))
  (fprintf s "~a" (prefill-content prefill))
  (get-output-string s))

(define (mistral/new messages)
  (define s (open-output-string))
  (define-values (msgs prefill) (split-messages messages))
  (define last (last-msg msgs))
  (for ([(role content) (in-messages msgs)]
        [msg (in-list msgs)])
    (cond
      [(string=? role "system") (fprintf s "[SYSTEM_PROMPT] ~a[/SYSTEM_PROMPT]" content)]
      [(string=? role "assistant") (fprintf s "~a</s>" content)]
      [(string=? role "tool") (fprintf s "~a" content)]
      [else
       (when (eq? last msg)
         (define tools (current-tools-string))
         (when tools
           (fprintf s "[AVAILABLE_TOOLS] ~a[/AVAILABLE_TOOLS]" tools)))
       (fprintf s "[INST] ~a[/INST]" content)]))
  (fprintf s "~a" (prefill-content prefill))
  (get-output-string s))
