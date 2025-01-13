#lang racket
(require racket/gui/base "../private/history.rkt")
(provide edit-history!)

(define (edit-history!)
  (cond
    [(edit-history (current-history))
     =>
     current-history]))

(define (edit-history initial-data)
  (define dialog (new dialog% [label "History Editor"] [width 480] [height 640]
                      [style '(resize-border close-button)]))

  (define items initial-data)
  
  (define (update-list-box)
    (send list-box set
          (for/list ([h (in-list items)])
            (hash-ref h 'role))
          (for/list ([h (in-list items)])
            (define str (hash-ref h 'content))
            (cond
              [(label-string? str) str]
              [else
               (string-append (substring str 0 197) "...")]))))

  (define list-box (new list-box% [parent dialog] [label "History"]
                        [style '(single column-headers)]
                        [choices '()]
                        [columns (list "Role" "Content")]
                        [callback (λ (b e) (on-selection-change))]))

  (define role-field (new text-field% [parent dialog] [label "Role: "] [init-value "user"]))
  (define content-field
    (new text-field%
         [parent dialog]
         [label "Content: "]
         [init-value ""]
         [style '(multiple)]
         [stretchable-height #f]))

  (define (on-selection-change)
    (define selected-index (send list-box get-selection))
    (when (and selected-index (<= 0 selected-index (sub1 (length items))))
      (define selected-item (list-ref items selected-index))
      (send role-field set-value (hash-ref selected-item 'role))
      (send content-field set-value (hash-ref selected-item 'content))))

  (define (add-item)
    (define role (cond
                   [(null? items) "user"]
                   [(string=? "user" (hash-ref (last items) 'role))
                    "assistant"]
                   [else "user"]))
    (set! items (append items (list (hasheq 'role role 'content ""))))
    (update-list-box)
    (send list-box set-selection (- (length items) 1))
    (on-selection-change))

  (define (delete-item)
    (define selected-index (send list-box get-selection))
    (when (and selected-index (<= 0 selected-index (sub1 (length items))))
      (set! items (append (take items selected-index) (drop items (add1 selected-index))))
      (update-list-box)
      (send list-box set-selection
            (if (= selected-index (length items))
                (sub1 selected-index)
                selected-index))
      (on-selection-change)))

  (define (save-item)
    (define selected-index (send list-box get-selection))
    (when (and selected-index (<= 0 selected-index (sub1 (length items))))
      (define role (send role-field get-value))
      (define content (send content-field get-value))
      (set! items (list-set items selected-index
                            (hasheq 'role role 'content content)))
      (update-list-box)
      (send list-box set-selection selected-index)
      (on-selection-change)))

  (define h-panel (new horizontal-pane% [parent dialog] [stretchable-height #f]))

  (define add-button (new button% [parent h-panel] [label "Add Item"] [callback (λ (b e) (add-item))]))
  (define delete-button (new button% [parent h-panel] [label "Delete Item"] [callback (λ (b e) (delete-item))]))
  (define save-button (new button% [parent h-panel] [label "Save Item"] [callback (λ (b e) (save-item))]))

  (define ok? #f)
  (define ok-button (new button% [parent dialog] [label "OK"]
                         [callback (λ (b e)
                                     (set! ok? #t)
                                     (send dialog show #f))]))
  (update-list-box)

  (send dialog show #t)
  (if ok?
      items
      #f))