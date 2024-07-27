#lang racket/base
(require racket/class net/base64 racket/file racket/lazy-require)
(lazy-require [racket/draw (bitmap%)] [racket/snip (image-snip%)])
(provide get-image)

(define (get-image img)
  (define (bitmap->bytes bm)
    (define b (open-output-bytes))
    (send bm save-file b 'jpeg)
    (bytes->string/latin-1
     (base64-encode 
      (get-output-bytes b)
      "")))
  (cond
    [(path? img)
     (bytes->string/latin-1 (base64-encode (file->bytes img) ""))]
    [else
     (cond
       [(is-a? img bitmap%) (bitmap->bytes img)]
       [(is-a? img image-snip%) (bitmap->bytes (send img get-bitmap))]
       [else (error 'get-image)])]))