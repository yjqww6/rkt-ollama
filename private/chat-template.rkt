#lang racket/base
(require racket/list racket/match racket/sequence racket/string)
(provide chat-template current-tools-string skip-cot-tokens)
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

(define (open-output)
  (box '()))
(define (push output v #:special? [special? #f])
  (set-box! output (cons (if special? (box v) v) (unbox output))))
(define (get-output output)
  (reverse (unbox output)))

(define (chatml messages)
  (define s (open-output))
  (define-values (msgs prefill) (split-messages messages))
  (for ([(role content) (in-messages msgs)])
    (push s (format "<|im_start|>~a\n" role) #:special? #t)
    (push s content)
    (push s "<|im_end|>\n" #:special? #t))
  (push s "<|im_start|>assistant\n" #:special? #t)
  (when prefill
    (push s (prefill-content prefill)))
  (get-output s))

(define (phi4 messages)
  (define s (open-output))
  (define-values (msgs prefill) (split-messages messages))
  (for ([(role content) (in-messages msgs)])
    (push s (format "<|im_start|>~a<|im_sep|>" role) #:special? #t)
    (push s content)
    (push s "<|im_end|>" #:special? #t))
  (push s "<|im_start|>assistant<|im_sep|>" #:special? #t)
  (when prefill
    (push s (prefill-content prefill)))
  (get-output s))

(define (llama3 messages)
  (define s (open-output))
  (define-values (msgs prefill) (split-messages messages))
  (for ([(role content) (in-messages msgs)])
    (push s (format "<|start_header_id|>~a<|end_header_id|>\n\n" role) #:special? #t)
    (push s content)
    (push s "<|eot_id|>" #:special? #t))
  (push s "<|start_header_id|>assistant<|end_header_id|>\n\n" #:special? #t)
  (when prefill
    (push s (prefill-content prefill)))
  (get-output s))

(define (gemma2 messages)
  (define s (open-output))
  (define-values (sys msgs prefill) (split-messages messages #f))
  (for ([(role content) (in-messages (inject-system-to-first-user msgs sys) #:assistant "model")])
    (push s (format "<start_of_turn>~a\n" role) #:special? #t)
    (push s content)
    (push s "<end_of_turn>\n" #:special? #t))
  (push s "<start_of_turn>model\n" #:special? #t)
  (when prefill
    (push s (prefill-content prefill)))
  (get-output s))

(define (skip-cot-tokens msgs [sep "</think>"])
  (for/list ([m (in-list msgs)])
    (match m
      [(hash 'role "assistant" 'content content #:open)
       #:when (string-contains? content sep)
       (hash-set m 'content
                 (last (string-split content sep)))]
      [else m])))

(define (deepseek3 messages)
  (define s (open-output))
  (define-values (sys msgs prefill) (split-messages messages #f))
  (when sys
    (push s (hash-ref sys 'content)))
  (for ([(role content) (in-messages (skip-cot-tokens msgs))])
    (cond
      [(string=? role "user")
       (push s "<｜User｜>" #:special? #t)
       (push s content)]
      [(string=? role "assistant")
       (push s "<｜Assistant｜>" #:special? #t)
       (push s content)
       (push s "<｜end▁of▁sentence｜>" #:special? #t)]))
  (push s "<｜Assistant｜>" #:special? #t)
  (when prefill
    (push s (prefill-content prefill)))
  (get-output s))

(define (last-msg msgs)
  (if (null? msgs) #f (last msgs)))

(define current-tools-string (make-parameter #f))

(define (mistral/internal messages v7? tekken?)
  (define merge-sys? (not v7?))
  (define leading-space (if tekken? "" " "))
  (define s (open-output))
  (define-values (sys msgs prefill)
    (if merge-sys?
        (let-values ([(msgs prefill) (split-messages messages #t)])
          (values #f msgs prefill))
        (split-messages messages #f)))
  (define new-msgs (inject-system-to-first-user msgs sys))
  (define last (last-msg new-msgs))
  (for ([(role content) (in-messages new-msgs)]
        [msg (in-list new-msgs)])
    (cond
      [(string=? role "system")
       (push s "[SYSTEM_PROMPT]" #:special? #t)
       (push s (format " ~a" content))
       (push s "[/SYSTEM_PROMPT]" #:special? #t)]
      [(string=? role "assistant")
       (push s content)
       (push s "</s>" #:special? #t)]
      [(string=? role "tool")
       (push s "[TOOL_RESULTS]" #:special? #t)
       (push s (format "~a~a" leading-space content))
       (push s "[/TOOL_RESULTS]" #:special? #t)]
      [else
       (when (eq? last msg)
         (define tools (current-tools-string))
         (when tools
           (push s "[AVAILABLE_TOOLS]" #:special? #t)
           (push s (format "~a~a" leading-space tools))
           (push s "[/AVAILABLE_TOOLS]" #:special? #t)))
       (push s "[INST]" #:special? #t)
       (push s (format "~a~a" leading-space content))
       (push s "[/INST]" #:special? #t)]))
  (when prefill
    (push s (prefill-content prefill)))
  (get-output s))

(define (mistral msgs)
  (mistral/internal msgs #f #f))

(define (mistral/v7 msgs)
  (mistral/internal msgs #t #f))

(define (mistral/v3/tekken msgs)
  (mistral/internal msgs #f #t))

(define (chat-template template-name)
  (match template-name
    ["chatml" chatml]
    ["phi4" phi4]
    ["llama3" llama3]
    ["gemma2" gemma2]
    ["mistral" mistral]
    ["mistral/v7" mistral/v7]
    ["mistral/v3/tekken" mistral/v3/tekken]
    ["deepseek3" deepseek3]
    [else (error "Unknown template name: ~a" template-name)]))
