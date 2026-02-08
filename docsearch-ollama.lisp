(in-package :docsearch-ollama)

(defvar *ollama-model* "qwen2.5-coder:7b")

(defun query-symbol-embeddings (query &key (top 5) threshold)
  (declare (optimize debug))
  (assert (not (and top threshold))
          (top threshold)
          "Only one of TOP or THRESHOLD should be supplied!")
  (let ((query-embedding (aref (optimize-embeddings (embed query)) 0))
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
          :do (write-string (symbol-documentation (parse-symbol-name s)))
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
  (let* ((full-query (print (apply #'uiop:strcat "In the context of Common Lisp, briefly answer: "
                                   queries)))
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
