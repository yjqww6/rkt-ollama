#lang racket/base
(require "../tool.rkt"
         racket/string racket/match json)
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
    (with-handlers ([exn:fail? (λ (e) #f)])
      (match (string->jsexpr calls)
        [(and c (list (hash* ['name _] ['arguments _]) ...)) c]
        [(list (hash* ['name ns] ['parameters ps]) ...)
         (for/list ([n (in-list ns)]
                    [p (in-list ps)])
           (hasheq 'name n 'arguments p))]
        [else #f])))
  (match (regexp-match #px"\\[TOOL_CALLS\\]\\s*(.*)" response)
    [(cons _ (list calls))
     (parse calls)]
    [else (parse response)]))

(define (make-mistral-response response)
  (jsexpr->string (hasheq 'content response)))

(define (make-llama3-system-template system)
  (string-append
   (if system system "")
   (if system "\n\n" "")
   #<<TPL
Cutting Knowledge Date: December 2023

When you receive a tool call response, use the output to format an answer to the orginal user question.

You are a helpful assistant with tool calling capabilities.
TPL
   ))
(define (make-llama3-prompt tools user)
  (format
   "~a\n~a\nQuestion: ~a"
   #<<TPL
Given the following functions, please respond with a JSON for a function call with its proper arguments that best answers the given prompt.

Respond in the format {"name": function name, "parameters": dictionary of argument name and its value}. Do not use variables.
TPL
   (string-join (map tool->string tools) "\n")
   user))
(define (parse-llama3-toolcall response)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (match (string->jsexpr response)
      [(hash 'name name 'parameters arguments)
       (list (hasheq 'name name 'arguments arguments))]
      [else #f])))
