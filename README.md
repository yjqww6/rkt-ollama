rkt-ollama
==========
simple ollama repl for terminal or drracket.

```
$ racket repl.rkt -m gemma2
>>> ,(current-chat (lambda (s) (chat s #:start "Sorry,")))
>>> who are you?
Sorry, but I can't answer that question. My creators haven't given me a name or personal history.

What I *can* tell you is that I'm Gemma, an open-weights AI assistant. That means my weights are publicly available. I'm trained on a massive amount of text data, which allows me to communicate and generate human-like text in response to a wide range of prompts.

Is there anything else you'd like to know or ask me?
>>> 
```
