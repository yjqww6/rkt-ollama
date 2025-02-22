#lang racket/base
(require json
         racket/match
         racket/string
         "../tool.rkt")
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

;;; another llama tools

(define (make-llama-system-template tools system)
  (format
   #<<TPL
You have access to the following functions:

~a

If a you choose to call a function ONLY reply in the following format:
<{start_tag}={function_name}>{parameters}{end_tag}
where

start_tag => `<function`
parameters => a JSON dict with the function argument name as key and function argument value as value.
end_tag => `</function>`

Here is an example,
<function=example_function_name>{"example_name": "example_value"}</function>

Reminder:
- Function calls MUST follow the specified format
- Required parameters MUST be specified
- Only call one function at a time
- Put the entire function call reply on one line
- Always add your sources when using search results to answer the user query

~a
TPL
   (string-join
    (for/list ([tool (in-list tools)])
      (match-define (obj 'function (and h (obj 'name name 'description desc)))
        (tool-desc tool))
      (format "Use the function '~a' to: ~a\n~a" name desc (djson->string h)))
    "\n")
   (or system "")))

(define (parse-llama-toolcall response)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (match (regexp-match* #px"<function=(\\w+)>\\s*(.*?)\\s*</function>" response #:match-select cdr)
      [(list (list names args) ...)
       (for/list ([name (in-list names)]
                  [arg (in-list args)])
         (hasheq 'name name 'arguments (string->jsexpr arg)))]
      [s #f])))
