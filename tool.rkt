#lang racket/base
(require (for-syntax racket/base)
         racket/match
         racket/string
         syntax/parse/define
         "private/json.rkt")
(provide define-tool tools-callback tool->string tools->string tool-name (struct-out tool) obj djson->string)

(struct tool (proc desc) #:property prop:procedure (struct-field-index proc))

(begin-for-syntax
  (define-syntax-class Param #:datum-literals (:)
    (pattern [Name:id : Type:id
                      (~alt
                       (~once (~seq #:desc Desc:string))
                       (~optional (~seq #:enum Enum:expr))
                       (~optional (~and (~seq #:def E:expr)
                                        (~bind [(Def 1) (list #'(λ () E))]
                                               [(Required 1) '()]))
                                  #:defaults ([(Def 1) '()]
                                              [(Required 1) (list #'Name)]))
                       (~optional (~seq #:items Items:expr)))
                      ...]
      #:with Prop
      #'(make-obj 'type (symbol->string 'Type)
                  'description Desc
                  (~? (~@ 'enum Enum))
                  (~? (~@ 'items Items))))))

(define-syntax-parser define-tool
  [(_ (Tool:id Param:Param ...) #:desc Desc:expr Body:expr ...+)
   #'(define Tool
       (tool
        (λ (arg)
          (define Param.Name (hash-ref arg 'Param.Name Param.Def ...)) ...
          Body ...)
        (make-obj
         'type "function"
         'function
         (make-obj 'name (symbol->string 'Tool)
                   'description Desc
                   'parameters
                   (make-obj 'type "object"
                             'properties (make-obj (~@ 'Param.Name Param.Prop) ...)
                             'required (map symbol->string (list 'Param.Required ... ...)))))))])

(define (tool-name tool)
  (obj-ref (obj-ref (tool-desc tool) 'function) 'name))

(define (tools-callback tools)
  (define m
    (for/hash ([tool (in-list tools)])
      (define name (obj-ref (obj-ref (tool-desc tool) 'function) 'name))
      (values name tool)))
  (λ (call)
    ((hash-ref m (hash-ref call 'name)) (hash-ref call 'arguments))))

(define (tool->string tool)
  (djson->string (tool-desc tool)))

(define (tools->string tools)
  (djson->string (map tool-desc tools)))
