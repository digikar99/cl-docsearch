(defpackage :docsearch-ollama
  (:use :cl)
  (:import-from :docsearch
                #:docsearch
                #:*symbol-doc-types*)
  (:export #:*ollama-url*
           #:*ollama-model*
           #:*ollama-embedding-model*

           #:*package-embedding-url-alist*
           #:*package-embedding-local-directory*

           #:ensure-package-symbol-embeddings
           #:query-symbol-embeddings
           #:query
           #:ask
           #:ask*))
