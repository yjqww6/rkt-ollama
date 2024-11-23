#lang racket/gui

(require "../main.rkt")
(use-llama-cpp)
(default-endpoint (cons "localhost" 8080))
(current-message-style #f)
(current-stop '("\n"))

(define frame (new frame% [label "Text Completion"] [width 600] [height 400]))

(define text (new text% [auto-wrap #t]))
((current-text-keymap-initializer) (send text get-keymap))
(define text-editor-canvas
  (new editor-canvas%
       [parent frame] [editor text] [stretchable-width #t] [stretchable-height #t]))

(define cust (box #f))

(define (stop-complete)
  (define c (unbox cust))
  (when c
    (custodian-shutdown-all c)
    (set-box! cust #f)))

(define (complete-callback button event)
  (stop-complete)
  (define txt (send text get-text))
  (define c (make-custodian))
  (set-box! cust c)
  (define-values (i o) (make-pipe))
  (parameterize ([current-custodian c])
    (thread (lambda ()
              (completion #:output o txt)
              (close-output-port o)))
    (thread (lambda ()
              (let loop ()
                (match (read-char i)
                  [(? eof-object?) (close-input-port i)]
                  [c
                   (queue-callback
                    (λ ()
                      (send text insert c (send text last-position))))
                   (loop)]))))))

(define button-panel
  (new horizontal-panel%
       [parent frame] [alignment '(center center)]
       [stretchable-width #f] [stretchable-height #f]))

(define complete-button
  (new button% [parent button-panel] [label "Complete"] [callback complete-callback]))
(define stop-button
  (new button% [parent button-panel] [label "Stop"] [callback (λ (b e) (stop-complete))]))

(send frame show #t)
