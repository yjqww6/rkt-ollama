#lang racket
(require racket/gui/base framework "../private/history.rkt")
(provide edit-history!)

(define (edit-history!)
  (cond
    [(edit-history (current-history))
     =>
     current-history]))

(define (edit-history initial-data)
  (define dialog (new dialog%
                     [label "History Editor"]
                     [width 480]
                     [height 640]
                     [style '(resize-border close-button)]))

  (define main-panel (new panel:vertical-dragable% [parent dialog]))

  (define items initial-data)
  (define ok? #f)

  (define list-box (new list-box%
                       [parent main-panel]
                       [label "History"]
                       [style '(single column-headers)]
                       [columns (list "Role" "Content")]
                       [callback (λ (b e) (on-selection-change))]
                       [choices '()]
                       [stretchable-height #t]))

  (define down-pane (new vertical-pane% [parent main-panel]))

  (define role-field (new text-field%
                          [parent down-pane]
                          [label "Role: "]
                          [init-value "user"]))

  (define content-field (new text-field%
                            [parent down-pane]
                            [label "Content: "]
                            [init-value ""]
                            [style '(multiple)]
                            [stretchable-height #t]))

  (define h-panel (new horizontal-pane%
                      [parent down-pane]
                      [stretchable-height #f]
                      [alignment '(center center)]))

  (define insert-button (new button%
                            [parent h-panel]
                            [label "Insert Item"]
                            [callback (λ (b e) (insert-item))]))

  (define add-button (new button%
                         [parent h-panel]
                         [label "Add Item"]
                         [callback (λ (b e) (add-item))]))

  (define delete-button (new button%
                            [parent h-panel]
                            [label "Delete Item"]
                            [callback (λ (b e) (delete-item))]))

  (define save-button (new button%
                          [parent h-panel]
                          [label "Save Item"]
                          [callback (λ (b e) (save-item))]))

  (define-values (ok-button cancel-button)
    (gui-utils:ok/cancel-buttons
     dialog
     (λ (b e) (set! ok? #t) (send dialog show #f))
     (λ (b e) (send dialog show #f))
     "OK"
     "Cancel"))

  (define (update-list-box)
    (define roles (map (λ (h) (hash-ref h 'role)) items))
    (define contents (map (λ (h)
                            (gui-utils:quote-literal-label (hash-ref h 'content)
                                                           #:quote-amp? #t))
                          items))
    (send list-box set roles contents))

  (define (on-selection-change)
    (define idx (send list-box get-selection))
    (when (and idx (<= 0 idx (length items)))
      (define item (list-ref items idx))
      (send role-field set-value (hash-ref item 'role))
      (send content-field set-value (hash-ref item 'content))))

  (define (add-item)
    (define role (if (and (not (empty? items))
                          (string=? "user" (hash-ref (last items) 'role)))
                     "assistant"
                     "user"))
    (set! items (append items (list (hasheq 'role role 'content ""))))
    (update-list-box)
    (send list-box set-selection (sub1 (length items)))
    (on-selection-change))

  (define (insert-item)
    (define idx (send list-box get-selection))
    (when (and idx (<= 0 idx (sub1 (length items))))
      (define new-item (hasheq 'role "user" 'content ""))
      (set! items (list-insert items idx new-item))
      (update-list-box)
      (send list-box set-selection idx)
      (on-selection-change)))

  (define (delete-item)
    (define idx (send list-box get-selection))
    (when (and idx (<= 0 idx (sub1 (length items))))
      (set! items (remove-item items idx))
      (update-list-box)
      (define new-idx (if (= idx (length items)) (sub1 idx) idx))
      (send list-box set-selection new-idx)
      (on-selection-change)))

  (define (save-item)
    (define idx (send list-box get-selection))
    (when (and idx (<= 0 idx (sub1 (length items))))
      (define new-item (hasheq 'role (send role-field get-value)
                               'content (send content-field get-value)))
      (set! items (list-set items idx new-item))
      (update-list-box)
      (send list-box set-selection idx)
      (on-selection-change)))

  (update-list-box)
  (send dialog show #t)
  (if ok?
      items
      #f))

(define (list-insert lst idx new-element)
  (append (take lst idx) (cons new-element (drop lst idx))))

(define (remove-item lst idx)
  (append (take lst idx) (drop lst (add1 idx))))