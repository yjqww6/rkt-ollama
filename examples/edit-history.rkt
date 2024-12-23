#lang racket
(require racket/gui/base "../private/history.rkt")
(provide edit-history!)

(define (edit-history!)
  (current-history
   (edit-history (current-history))))

(define (edit-history initial-data)
  (define dialog (new dialog% [label "History Editor"] [width 400] [height 300]
                      [style '(resize-border)]))

  (define items (list))

  (define (make-item-panel parent role content)
    (define main-panel (new vertical-panel% [parent parent] 
                            [alignment '(left center)] 
                            [border 2] 
                            [spacing 5]
                            [style '(border)]))
  
    (define top-panel (new horizontal-panel% [parent main-panel] [alignment '(left center)] [spacing 5]))
    (define role-field (new text-field% [parent top-panel] [label "Role: "] [init-value role]))
    (define delete-button (new button% [parent top-panel] [label "Delete"]
                               [callback (λ (b e) (delete-item main-panel))]))
  
    (define content-field (new text-field% [parent main-panel] [label ""]
                               [style '(multiple)] [min-width 200] [min-height 50] [init-value content]))
  
    (values main-panel role-field content-field))

  (define (add-item)
    (define last-role (if (null? items)
                          "user"
                          (send (cadr (last items)) get-value)))
    (define new-role (if (equal? last-role "assistant") "user" "assistant"))
    (define-values (main-panel role-field content-field) (make-item-panel item-list new-role ""))
    (set! items (append items (list (list main-panel role-field content-field)))))

  (define (delete-item main-panel)
    (send item-list delete-child main-panel)
    (set! items (filter (λ (item) (not (eq? (car item) main-panel))) items)))

  (define item-list (new vertical-panel% [parent (new vertical-panel% [parent dialog])]))

  (for-each (λ (item)
              (define-values (main-panel role-field content-field) (make-item-panel item-list (hash-ref item 'role) (hash-ref item 'content)))
              (set! items (append items (list (list main-panel role-field content-field)))))
            initial-data)

  (define add-button (new button% [parent dialog] [label "Add Item"] [callback (λ (b e) (add-item))]))

  (define (get-edited-items)
    (map (λ (item)
           (hasheq 'role (send (cadr item) get-value) 'content (send (caddr item) get-value)))
         items))

  (define ok-button (new button% [parent dialog] [label "OK"]
                         [callback (λ (b e) 
                                     (send dialog show #f))]))

  (send dialog show #t)

  (get-edited-items))
