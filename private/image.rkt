#lang racket/base
(require net/base64
         racket/class
         racket/file)
(provide get-image)

(define (lazy mod id)
  (define cell #f)
  (λ ()
    (unless cell
      (set! cell (dynamic-require mod id)))
    cell))
(define bitmap (lazy 'racket/draw 'bitmap%))
(define image-snip (lazy 'racket/snip 'image-snip%))

(define (get-image img)
  (define (bitmap->bytes bm)
    (define b (open-output-bytes))
    (send bm save-file b 'jpeg)
    (bytes->string/latin-1
     (base64-encode 
      (get-output-bytes b)
      "")))
  (cond
    [(path? img) (bytes->string/latin-1 (base64-encode (file->bytes img) ""))]
    [(bytes? img) (bytes->string/latin-1 (base64-encode img ""))]
    [(is-a? img (bitmap)) (bitmap->bytes img)]
    [(is-a? img (image-snip)) (bitmap->bytes (send img get-bitmap))]
    [else (error 'get-image)]))