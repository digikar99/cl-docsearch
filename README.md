# Docsearch

Search documentation of Common Lisp symbols in the current lisp image for a query string.

The base package and system is pure Common Lisp. An additional system [docsearch-ollama](./README-docsearch-ollama.md) is included for use with ollama.

### Example

To search for symbols related to "function" in all packages, you would call:

```lisp
(docsearch:docsearch "alist")
```

This will print out any symbols and their documentation that contain the word "alist".

To restrict the search to a certain package or packages, supply it as a second argument.

```lisp
(docsearch:docsearch "alist" :alexandria)
```

```
ALEXANDRIA:ALIST-HASH-TABLE
  Function:
    Returns a hash table containing the keys and values of the association list
    ALIST. Hash table is initialized using the HASH-TABLE-INITARGS.

ALEXANDRIA:ASSOC-VALUE
  Function:
    ASSOC-VALUE is an alist accessor very much like ASSOC, but it can
    be used with SETF.

ALEXANDRIA:RASSOC-VALUE
  Function:
    RASSOC-VALUE is an alist accessor very much like RASSOC, but it can
    be used with SETF.

ALEXANDRIA:HASH-TABLE-ALIST
ALEXANDRIA:PLIST-ALIST
ALEXANDRIA:ALIST-PLIST
  Function:
    Returns a property list containing the same keys and values as the
    association list ALIST in the same order.
```

## Installation

1. [quicklisp with https support](https://github.com/rudolfochrist/ql-https):

```
curl https://raw.githubusercontent.com/rudolfochrist/ql-https/master/install.sh | bash
```

2. [ultralisp](https://ultralisp.org):

```
(ql-dist:install-dist "https://dist.ultralisp.org/" :prompt nil)
```

3. [docsearch](https://github.com/digikar99/cl-docstrings):

```
(ql:quickload "docsearch")
```

4. (Optional) [more-docstrings](https://github.com/ciel-lang/more-docstrings):

```
(ql:quickload "more-docstrings")
```

## Related Projects

- [cl-livedocs](https://github.com/mmontone/quicklisp-apropos)
- [quicksearch](https://github.com/tkych/quicksearch)
- [quicklisp-apropos](https://github.com/mmontone/quicklisp-apropos)

## API Documentation

### Variables `*symbol-doc-types*`

A list of types of documentation to search within symbols. TODO: Expand this with in-nomine

### Variable `*search-function*`

The function used by `docsearch` for searching documentation strings. By default this uses `search` with `char-equal`.

### Function: `docsearch`

```lisp
Lambda list: (search-term &optional packages (mode :external) (method *search-function*))
```

Searches for Lisp symbols based on a given search string.

**Parameters:**

- `search-string` (string or symbol): The term to search for.
- `packages` (a single package-designator, list of package-designators, or a predicate function, optional): List of packages to search within. Defaults to all packages. If it is a function, it is used to narrow the search to the good packages of `(list-all-packages)`.
- `mode` (symbol, optional): Whether to search external or internal symbols. Options are `:external` and
`:internal`. Defaults to `:external`.
- `method` (function, optional): The function to use for searching documentation strings.

Example `packages` function to search for quicklisp related packages:

```lisp
(defun ql-package-p (pkg)
  (let ((name (package-name pkg)))
    (or (search "QL" name)
        (search "QUICKLISP" name))))
```

```lisp
(docsearch:docsearch "dist" #'ql-package-p)
```

**Returns:**

None
