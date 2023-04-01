# Common Lisp library for Documents Question Answering Using OpenAI GPT3 APIs

From my book URI: https://leanpub.com/lovinglisp

There is a **Makefile** in the repo https://github.com/mark-watson/loving-common-lisp that can be copied
to your **~/quicklisp/local-projects** directory. Then in **~/quicklisp/local-projects** run:

    make fetch

to get all of the library examples from my book.


## setting your OpenAI API key
 
 Define the  "OPENAI_KEY" environment variable with the value of your OpenAI API key
 
## Example:


## Embeddings (from 'openai' package)

```
(setf e1 (openai::embeddings "John bought a new car"))

(setf e2 (openai::embeddings "who bought what"))

CL-USER 23 > (openai::dot-product e1 e1)
0.9999999

CL-USER 24 > (openai::dot-product e1 e2)
0.7841768
```
