#lang racket/base
(require json
         racket/match
         racket/symbol
         (for-syntax racket/base syntax/parse))
(provide (struct-out inlined-json)
         make-obj obj-ref djson->string djson->bytes write-djson
         string->jsexpr read-json
         example->schema
         obj)

(struct inlined-json (data) #:transparent)
(struct ordered-obj (assocs) #:transparent)

(define (make-obj . args)
  (ordered-obj
   (let loop ([args args])
     (match args
       [(list* k v args) (cons (cons k v) (loop args))]
       ['() '()]))))

(define-match-expander obj
  (λ (stx)
    (syntax-parse stx
      [(_ (~seq K V) ...)
       #'(ordered-obj (and (app (λ (x) (assq K x)) (cons _ V))
                           ...))])))

(define (obj-ref obj k [fail (λ () #f)])
  (define p (assq k (ordered-obj-assocs obj)))
  (if p (cdr p) (fail)))

(define-syntax-rule (write-obj Seq Out)
  (let ([out Out])
    (write-char #\{ out)
    (for/fold ([first #t])
              ([p Seq])
      (unless first
        (write-char #\, out))
      (write-json (symbol->immutable-string (car p)) out)
      (write-char #\: out)
      (write-djson (cdr p) out)
      #f)
    (write-char #\} out)))

(define-syntax-rule (write-list Seq Out)
  (let ([out Out])
    (write-char #\[ out)
    (for/fold ([first #t])
              ([d Seq])
      (unless first
        (write-char #\, out))
      (write-djson d out)
      #f)
    (write-char #\] out)))

(define (write-djson d out)
  (match d
    [(inlined-json data) (write-string data out)]
    [(ordered-obj assocs) (write-obj (in-list assocs) out)]
    [(? hash? h) (write-obj (in-hash-pairs h) out)]
    [(? list? l) (write-list (in-list l) out)]
    [(? vector? v) (write-list (in-vector v) out)]
    [else
     (write-json d out)]))

(define (djson->string d)
  (define o (open-output-string))
  (write-djson d o)
  (get-output-string o))

(define (djson->bytes d)
  (define o (open-output-bytes))
  (write-djson d o)
  (get-output-bytes o))

(define (example->schema d)
  (match d
    [(? string?) (make-obj 'type "string")]
    [(? number?) (make-obj 'type "number")]
    [(? boolean?) (make-obj 'type "bool")]
    [(inlined-json data) data]
    [(list a) (make-obj'type "array" 'items (example->schema a))]
    [(or (and (? hash?) (app hash-keys ks) (app hash-values vs))
         (ordered-obj (list (cons ks vs) ...)))
     (if (null? ks)
         (make-obj 'type "object")
         (make-obj
          'type "object"
          'properties
          (ordered-obj
           (for/list ([k (in-list ks)]
                      [v (in-list vs)])
             (cons k
                   (example->schema v))))
          'required (map symbol->immutable-string ks)))]
    [(== (json-null)) (make-obj 'type "null")]))
