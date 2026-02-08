# docsearch-ollama

[docsearch-ollama](https://github.com/digikar99/cl-docsearch/) provides Common Lisp documentation search functionality through Ollama. It computes and caches embeddings corresponding to symbol documentation, and looks up user queries by comparing the cosine similarity of the query embedding with symbol documentation embeddings.

In simpler words, this means, you can do something like:


```lisp
(query "How do I remove whitespace from the ends of a string?")
```

```
COMMON-LISP:STRING-TRIM

COMMON-LISP:STRING-RIGHT-TRIM

COMMON-LISP:STRING-LEFT-TRIM

COMMON-LISP:READ-PRESERVING-WHITESPACE
  When it is used as a function, it takes the following arguments: (&OPTIONAL
                                                                    (STREAM
                                                                     *STANDARD-INPUT*)
                                                                    (EOF-ERROR-P
                                                                     T)
                                                                    (EOF-VALUE
                                                                     NIL)
                                                                    (RECURSIVE-P
                                                                     NIL))
    Read from STREAM and return the value read, preserving any whitespace
       that followed the object.

COMMON-LISP:CLEAR-INPUT
```

Or even:

```lisp
(ask "How do I remove whitespace from the ends of a string?")
```

> To remove whitespace from the ends of a string in Common Lisp, you can use the `STRING-TRIM`, `STRING-RIGHT-TRIM`, and `STRING-LEFT-TRIM` functions. These functions respectively trim whitespace from both ends, just the right end, and just the left end of the string.

## Installation

1. Set up ollama: https://ollama.com/download

2. Download a standard model and an embedding model. For example:

    ```
    ollama pull qwen-2.5-coder:7b
    ollama pull qwen3-embedding:0.6b
    ```

3. Follow install instructions for [docsearch](README.md) and install ultralisp

4. Quickload docsearch-ollama:

```lisp
(ql:quickload :docsearch-ollama)
```

## Configuration

The following variables can be configured in your Lisp environment before using the package:

- `*ollama-url*`: URL of the Ollama API.
- `*ollama-model*`: Model to use for embedding and generating responses.
- `*ollama-embedding-model*`: Embedding model to use.
- `*package-embedding-url-prefix*`: Default URL prefix used for retrieving embeddings
- `*package-embedding-url-alist*` (currently unused): Packages whose embedding must be fetched from a different source than `*package-embedding-url-prefix*`
- `*package-embedding-local-directory*`: Location where embeddings are stored locally

Default configuration:

```lisp
(setf *ollama-url* "http://localhost:11434")
(setf *ollama-model* "qwen2.5-coder:7b")
(setf *ollama-embedding-model* "qwen3-embedding:0.6b")
(setf *package-embedding-local-directory*
      (merge-pathnames "embeddings/" (asdf:component-pathname (asdf:find-system "docsearch-ollama")))]
```

## Usage

1. `docsearch-ollama` must prepare an "index" mapping symbols to the embeddings of their documentation. Embeddings are basically a long vector of numbers (1024, 2560, etc length). This is done on a per-package basis. This can take a while to run. It will be faster to run if the embeddings are available [here](https://github.com/digikar99/cl-docsearch/releases/tag/qwen3-embedding-0.6b).

    ```lisp
    (ensure-package-symbol-embeddings :cl)
    ```

    Once generated or downloaded, these embeddings will be stored at the location given by `*package-embedding-local-directory*`.

2. Retrieve relevant symbols and their documentation based on cosine similarity to the query embedding.

    ```lisp
    (query "How do I remove whitespace from the ends of a string?")
    ```

    ```
    COMMON-LISP:STRING-TRIM

    COMMON-LISP:STRING-RIGHT-TRIM

    COMMON-LISP:STRING-LEFT-TRIM

    COMMON-LISP:READ-PRESERVING-WHITESPACE
      When it is used as a function, it takes the following arguments: (&OPTIONAL
                                                                        (STREAM
                                                                         *STANDARD-INPUT*)
                                                                        (EOF-ERROR-P
                                                                         T)
                                                                        (EOF-VALUE
                                                                         NIL)
                                                                        (RECURSIVE-P
                                                                         NIL))
        Read from STREAM and return the value read, preserving any whitespace
           that followed the object.

    COMMON-LISP:CLEAR-INPUT
    ```

3. Ask Questions. These will be answered on the basis of the symbols and documentation returned by `query`.

    ```lisp
    (ask "How do I remove whitespace from the ends of a string?")
    ```

    > To remove whitespace from the ends of a string in Common Lisp, you can use the `STRING-TRIM`, `STRING-RIGHT-TRIM`, and `STRING-LEFT-TRIM` functions. These functions respectively trim whitespace from both ends, just the right end, and just the left end of the string.

    `query` and `ask` also take `:top` and `:threshold` parameters that can be used to modify the criteria used to filter the embeddings.

4. Directly query the Ollama model without referencing any local symbols.

    ```lisp
    (ask* "What is the purpose of `mapcar` in Common Lisp?")
    ```

    > `mapcar` applies a function to each element of one or more lists and returns a new list containing the results.


- You can customize the models used for embedding and generating responses by setting `*ollama-model*` and `*ollama-embedding-model*`.
-
