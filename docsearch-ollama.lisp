(defpackage :docsearch-ollama
  (:use :cl)
  (:import-from :docsearch
                #:docsearch
                #:*symbol-doc-types*)
  (:export #:*ollama-url*
           #:*ollama-model*
           #:*ollama-embedding-model*
           #:symbol-documentation
           #:populate-package-symbol-embeddings
           #:query-symbol-embeddings
           #:query
           #:ask
           #:ask*))

(in-package :docsearch-ollama)

(defvar *ollama-url* "http://localhost:11434")

(defvar *ollama-model* "qwen2.5-coder:7b")

(defvar *ollama-embedding-model*
  ;; "qwen3-embedding:4b"
  "qwen3-embedding:0.6b")

(defun embed (inputs &optional return-map)
  (let* ((inputs (etypecase inputs
                   (cons inputs)
                   (string (list inputs))))
         (response
           (dexador:post (uiop:strcat *ollama-url* "/api/embed")
                         :connect-timeout nil
                         :read-timeout nil
                         :content (shasht:write-json (alexandria:alist-hash-table
                                                      (list (cons "model" *ollama-embedding-model*)
                                                            (cons "input" inputs)
                                                            (cons "stream" nil)))
                                                     nil)))
         (embeddings
           (gethash "embeddings" (shasht:read-json response))))
    (if (not return-map)
        (loop :for embedding :across embeddings
              :collect (make-array (length embedding) :element-type 'single-float
                                                      :initial-contents embedding))
        (loop :with embedding-map := (make-hash-table :test #'equal)
              :for input :in inputs
              :for embedding :across embeddings
              :do (setf (gethash input embedding-map)
                        (make-array (length embedding) :element-type 'single-float
                                                       :initial-contents embedding))
              :finally (return embedding-map)))))

(defun cosine-sim (embedding-1 embedding-2)
  "Assumes the embeddings are L2 normalized, thus, of length 1."
  (declare (type (simple-array single-float 1) embedding-1 embedding-2)
           (optimize speed))
  (loop :with sum :of-type single-float := 0.0f0
        :for i :below (array-dimension embedding-1 0)
        :do (incf sum (* (aref embedding-1 i) (aref embedding-2 i)))
        :finally (return sum)))

(defvar *symbol-embeddings* (make-hash-table :test #'eq))

(defun symbol-documentation (symbol &optional (prefix ""))
  (with-output-to-string (*standard-output*)
    (format t "~A~A:~A"
            prefix
            (package-name (symbol-package symbol))
            symbol)
    (terpri)
    (pprint-logical-block (*standard-output* nil :per-line-prefix "  ")
      (loop :for doc-type :in *symbol-doc-types*
            :for doc := (documentation symbol doc-type)
            :if doc
            :do (write-string "When it is used as a ")
                (cond ((eq 'function doc-type)
                       (handler-case
                           (let ((arglist (trivial-arguments:arglist symbol)))
                             (format t "function, it takes the following arguments: ~A"
                                     arglist))
                         (error ()
                           (format nil "~A: " doc-type))))
                      ((eq 'setf doc-type)
                       (handler-case
                           (let ((arglist (trivial-arguments:arglist `(setf ,symbol))))
                             (format t "setf-function, it takes the following arguments: ~A"
                                     arglist))
                         (error ()
                           (format nil "~A: " doc-type)))
                       )
                      (t
                       (format nil "~A: " doc-type)))
                (terpri)
                (pprint-logical-block (*standard-output* nil :per-line-prefix "  ")
                  (write-string doc))
                (terpri)))))

(defun populate-package-symbol-embeddings (package)
  (declare (optimize debug))
  (let* ((symbols (let ((symbols))
                    (do-external-symbols (s package)
                      (push s symbols))
                    symbols))
         (docstrings (loop :for s :in symbols
                           :collect (symbol-documentation s "Following is the documentation for ")))
         (embeddings (embed docstrings)))
    (loop :for s :in symbols
          :for e :in embeddings
          :do (setf (gethash s *symbol-embeddings*) e))
    *symbol-embeddings*))

(defun query-symbol-embeddings (query &key (top 5) threshold)
  (declare (optimize debug))
  (assert (not (and top threshold))
          (top threshold)
          "Only one of TOP or THRESHOLD should be supplied!")
  (let ((query-embedding (first (embed query)))
        (results nil))
    (maphash (lambda (symbol embedding)
               (push (cons symbol (cosine-sim embedding query-embedding))
                     results))
             *symbol-embeddings*)
    (setf results (sort results #'> :key #'cdr))
    (cond (top
           (subseq results 0 top))
          (threshold
           (loop :for (s . sim) :in results
                 :while (>= sim threshold)
                 :collect (cons s sim))))))

(defun query (query &key (top 5) threshold)
  (let* ((relevant-symbols (mapcar #'first (query-symbol-embeddings query :top top :threshold threshold))))
    (loop :for s :in relevant-symbols
          :do (write-string (symbol-documentation s))
              (terpri))))

(defun ask (query &key (top 5) threshold doc)
  (let* ((relevant-symbols (mapcar #'first (query-symbol-embeddings query :top top :threshold threshold)))
         (full-query (apply #'uiop:strcat
                            (nconc (list* "Consider the documentation of the following Common Lisp symbols:"
                                          (string #\newline)
                                          (alexandria:mappend (lambda (s)
                                                                (list (symbol-documentation s)
                                                                      (string #\newline)
                                                                      (string #\newline)))
                                                              relevant-symbols))
                                   (list (string #\newline)
                                         "Based on this, very briefly, answer: "
                                         query))))
         (response
           (dexador:post (uiop:strcat *ollama-url* "/api/generate")
                         :connect-timeout nil
                         :read-timeout nil
                         :content (shasht:write-json (alexandria:alist-hash-table
                                                      (list (cons "model" *ollama-model*)
                                                            (cons "prompt" (progn
                                                                             (when doc
                                                                               (write-string full-query))
                                                                             full-query))
                                                            (cons "stream" nil)))
                                                     nil))))
    (terpri)
    (write-string (gethash "response" (shasht:read-json response)))
    (values)))

(defun ask* (&rest queries)
  "A version of ASK to query the model directly without any information from the current image."
  (let* ((full-query (apply #'uiop:strcat "In the context of Common Lisp, very briefly, answer: "
                            queries))
         (response
           (dexador:post (uiop:strcat *ollama-url* "/api/generate")
                         :connect-timeout nil
                         :read-timeout nil
                         :content (shasht:write-json (alexandria:alist-hash-table
                                                      (list (cons "model" *ollama-model*)
                                                            (cons "prompt" full-query)
                                                            (cons "stream" nil)))
                                                     nil))))
    (terpri)
    (write-string (gethash "response" (shasht:read-json response)))
    (values)))
