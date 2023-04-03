# Common Lisp library for Documents Question Answering Using OpenAI GPT3 APIs and a Local Embeddings Vector Database

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.

Note: this library is under construction, but the code as-is works and will hopefully be useful.

## Require's my openai library

You need to Quicklisp install:

    https://github.com/mark-watson/openai

## setting your OpenAI API key
 
 Define the  "OPENAI_KEY" environment variable with the value of your OpenAI API key
 
## Example:

```
$ sbcl
* (quicklisp:quickload :docs-qa)
To load "docs-qa":
  Load 1 ASDF system:
    docs-qa
; Loading "docs-qa"
..................................................
[package docs-qa]To load "sqlite":
  Load 1 ASDF system:
    sqlite
; Loading "sqlite"
.

#<sqlite-handle {7005CA3783}>
(:docs-qa)
* (in-package :docs-qa)
#<package "DOCS-QA">
* (test)

** query: What is the history of the science of chemistry?
** answer: The history of chemistry as a science began in the 6th century BC, when the Greek philosopher Leucippus and his student Democritus posited the existence of an endless number of worlds

** query: What are the advantages of engainging in sports?
** answer: The advantages of engaging in sports are:n1. It helps to develop the body and mind.n2. It helps to develop the character.n3. It helps to develop the personality.
```
