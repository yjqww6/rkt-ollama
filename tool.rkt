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
                       (~optional
                        (~and #:required (~bind [(Required 1) #'(Name)]))
                        #:defaults ([(Required 1) #'()]))
                       (~optional (~seq #:items Items:expr)))
                      ...]
      #:with Prop
      #'(hasheq 'type (symbol->string 'Type)
                'description Desc
                (~? (~@ 'enum Enum))
                (~? (~@ 'items Items))))))

(define-syntax-parser define-tool
  [(_ (Tool:id Param:Param ...) #:desc Desc:string Body:expr ...+)
   #'(define Tool
       (tool
        (λ (arg)
          (define Param.Name (hash-ref arg 'Param.Name)) ...
          Body ...)
        (hasheq
         'type "function"
         'function
         (hasheq 'name (symbol->string 'Tool)
                 'description Desc
                 'parameters
                 (hasheq 'type "object"
                         'properties (hasheq (~@ 'Param.Name Param.Prop) ...)
                         'required (list (symbol->string 'Param.Name) ...))))))])

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

;; example template
(module+ template
  (provide (all-defined-out))
  (define (make-nous-system-template tools system)
    (define prefix #<<TPL
# Tools

You may call one or more functions to assist with the user query.

You are provided with function signatures within <tools></tools> XML tags:
<tools>

TPL
      )
    (define suffix #<<TPL

</tools>

For each function call, return a json object with function name and arguments within <tool_call></tool_call> XML tags:
<tool_call>
{"name": <function-name>, "arguments": <args-json-object>}
</tool_call>
TPL
      )
    (cond
      [(null? tools) system]
      [else
       (string-append
        (if system system "")
        (if system "\n\n" "")
        prefix
        (string-join (map tool->string tools) "\n")
        suffix)]))

  (define (parse-nous-toolcall response)
    (match (regexp-match* #px"<tool_call>\\s*(.*?)\\s*</tool_call>" response #:match-select cadr)
      [(list call ...)
       (match (map string->jsexpr call)
         [(and c (list (hash* ['name _] ['arguments _]) ...)) c]
         [else #f])]
      [else #f]))

  (define (make-nous-response response)
    (string-append "<tool_response>" (jsexpr->string response) "</tool_response>"))

  (define (parse-mistral-toolcall response)
    (define (parse calls)
      (with-handlers ([exn:fail:read? (λ (e) #f)])
        (match (string->jsexpr calls)
          [(and c (list (hash* ['name _] ['arguments _]) ...)) c]
          [else #f])))
    (match (regexp-match #px"\\[TOOL_CALLS\\]\\s*(.*)" response)
      [(cons _ (list calls))
       (parse calls)]
      [else (parse response)]))

  (define (make-mistral-response response)
    (format "[TOOL_RESULTS] ~a[/TOOL_RESULTS]" (jsexpr->string (hasheq 'content response))))
  )
