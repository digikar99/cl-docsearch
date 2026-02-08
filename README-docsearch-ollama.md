# docsearch-ollama

This Common Lisp package documentation search functionality using Ollama. It allows querying symbol embeddings and retrieving relevant documentation based on cosine similarity. As well as answer questions based on these relevant symbols.

## Installation

1. Set up ollama: https://ollama.com/download

2. Download a standard model and an embedding model. For example:

    ```
    ollama pull qwen-2.5-coder:7b
    ollama pull qwen3-embedding:0.6b
    ```

3. Follow install instructions for [docsearch](README.md) to install ultralisp

4. Quickload docsearch-ollama:

```lisp
(ql:quickload :docsearch-ollama)
```

## Configuration

The following variables can be configured in your Lisp environment before using the package:

- `*ollama-url*`: URL of the Ollama API.
- `*ollama-model*`: Model to use for embedding and generating responses.
- `*ollama-embedding-model*`: Embedding model to use.

Default configuration:

```lisp
(setf *ollama-url* "http://localhost:11434")
(setf *ollama-model* "qwen2.5-coder:7b")
(setf *ollama-embedding-model* "qwen3-embedding:0.6b")
```

## Usage

1. Get a nicely formatted documentation for a symbol (thanks to [more-docstrings](https://github.com/ciel-lang/more-docstrings) for extra information!):

    ```lisp
    (symbol-documentation 'mapcar)
    ```

    ```lisp
    "COMMON-LISP:MAPCAR
      When it is used as a function, it takes the following arguments: (FUNCTION
                                                                        LIST &REST
                                                                        MORE-LISTS)
        Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
        Return list of FUNCTION return values.

        For example:

        (mapcar #'+ '(1 2 3) '(10 20 30))  ;; => (11 22 33)

        (mapcar (lambda (x)
                  (format t \"~a is ~R~&\" x x))
                '(1 2 3))
        ;; =>
        1 is one
        2 is two
        3 is three
        (NIL NIL NIL)

      "
    ```

2. Populate embeddings for all symbols in a given package. This can take a while to run. (Perhaps we can prepare an online per-package symbol embedding database that users can simply download?)

    ```lisp
    (populate-package-symbol-embeddings :cl)
    ```

3. Retrieve relevant symbols and their documentation based on cosine similarity to the query embedding.

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

4. Ask Questions. These will be answered on the basis of the symbols and documentation returned by `query`.

    ```lisp
    (ask "How do I remove whitespace from the ends of a string?")
    ```


    > To remove whitespace from the ends of a string in Common Lisp, you can use the `STRING-TRIM`, `STRING-RIGHT-TRIM`, and `STRING-LEFT-TRIM` functions. These functions respectively trim whitespace from both ends, just the right end, and just the left end of the string.

5. Directly query the Ollama model without referencing any local symbols.

    ```lisp
    (ask* "What is the purpose of `mapcar` in Common Lisp?")
    ```

    > `mapcar` applies a function to each element of one or more lists and returns a new list containing the results.


### Customization

- You can customize the models used for embedding and generating responses by setting `*ollama-model*` and `*ollama-embedding-model*`.
- Adjust the number of top results returned or filter by similarity threshold using the `:top` and `:threshold` parameters in the `query` and `ask` functions.
