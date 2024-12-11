#lang racket/base
(require racket/string racket/match json
         syntax/parse/define (for-syntax racket/base))
(provide define-tool tools-callback tool->string tools->string (struct-out tool))

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
      #'(hasheq 'type (symbol->string 'Type)
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
        (hasheq
         'type "function"
         'function
         (hasheq 'name (symbol->string 'Tool)
                 'description Desc
                 'parameters
                 (hasheq 'type "object"
                         'properties (hasheq (~@ 'Param.Name Param.Prop) ...)
                         'required (map symbol->string (list 'Param.Required ... ...)))))))])

(define (tools-callback tools)
  (define m
    (for/hash ([tool (in-list tools)])
      (define name (hash-ref (hash-ref (tool-desc tool) 'function) 'name))
      (values name tool)))
  (λ (call)
    ((hash-ref m (hash-ref call 'name)) (hash-ref call 'arguments))))

(define (tool->string tool)
  (match (tool-desc tool)
    [(hash 'type "function" 'function j)
     (format #<<TPL
{"type": "function", "function": ~a}
TPL
             (jsexpr->string j))]))

(define (tools->string tools)
  (if (null? tools)
      #f
      (string-join (map tool->string tools) ", " #:before-first "[" #:after-last "]")))
