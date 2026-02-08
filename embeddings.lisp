(in-package :docsearch-ollama)

(defun cosine-sim (embedding-1 embedding-2)
  "Assumes the embeddings are L2 normalized, thus, of length 1."
  (declare (type (simple-array single-float 1) embedding-1 embedding-2)
           (optimize speed))
  (loop :with sum :of-type single-float := 0.0f0
        :for i :below (array-dimension embedding-1 0)
        :do (incf sum (* (aref embedding-1 i) (aref embedding-2 i)))
        :finally (return sum)))


(defun optimize-embeddings (embeddings)
  (flet ((optimize-storage (embedding)
           (declare (type (array t 1) embedding)
                    (optimize speed))
           (make-array (length embedding)
                       :element-type 'single-float
                       :initial-contents embedding)))
    (etypecase embeddings
      (hash-table
       (maphash (lambda (name embedding)
                  (setf (gethash name embeddings)
                        (optimize-storage embedding)))
                embeddings))
      ((vector t)
       (dotimes (i (length embeddings))
         (setf (aref embeddings i)
               (optimize-storage (aref embeddings i)))))))
  embeddings)

(defvar *ollama-url* "http://localhost:11434")

(defvar *ollama-embedding-model*
  ;; "qwen3-embedding:4b"
  "qwen3-embedding:0.6b")

;; TODO: This is unused
(defvar *package-embedding-url-alist* nil
  "Packages whose embedding must be fetched from a different source than *PACKAGE-EMBEDDING-URL-PREFIX*")

(defvar *package-embedding-url-prefix* "https://github.com/digikar99/cl-docsearch/releases/download/"
  "Default URL prefix used for retrieving embeddings")

(defvar *package-embedding-local-directory*
  (merge-pathnames "embeddings/"
                   (asdf:component-pathname (asdf:find-system "docsearch-ollama"))))


(defvar *symbol-embeddings* (make-hash-table :test #'equal))

(defun package-symbol-embeddings-try-download (package embeddings-file)
  (handler-case
      (let* ((package (if (packagep package)
                          package
                          (find-package package)))
             (package-name (package-name package))
             (url (uiop:strcat *package-embedding-url-prefix*
                               (str:replace-all ":" "-" *ollama-embedding-model*)
                               "/"
                               package-name
                               ".json.gz"))
             ;; compressed embeddings stream
             (ce-stream (dexador:get url :want-stream t)))
        (with-open-file (out embeddings-file :direction :output
                                             :if-does-not-exist :create
                                             :element-type '(unsigned-byte 8))
          (chipz:decompress out 'chipz:gzip ce-stream))
        (shasht:read-json (alexandria:read-file-into-string embeddings-file)))
    (dexador:http-request-not-found nil)))

(defun embed (inputs)
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
                                                     nil))))
    (gethash "embeddings" (shasht:read-json response))))

(defun package-symbol-embeddings-compute (package embeddings-file)
  (let* ((symbols (let ((symbols))
                    (do-external-symbols (s package)
                      (push s symbols))
                    symbols))
         (docstrings (loop :for s :in symbols
                           :collect (symbol-documentation s "Following is the documentation for ")))
         (embeddings-vector (embed docstrings))
         (embeddings (make-hash-table :test #'equal)))
    (loop :for s :in symbols
          :for e :across embeddings-vector
          :do (setf (gethash (full-symbol-name s) embeddings) e))
    (with-standard-io-syntax
      (alexandria:write-string-into-file (shasht:write-json embeddings nil)
                                         embeddings-file
                                         :if-does-not-exist :create))
    embeddings))



(defun ensure-package-symbol-embeddings (package)
  (declare (optimize debug))

  (ensure-directories-exist *package-embedding-local-directory*)

  (let* ((name (ensure-package-name package))

         (embeddings-file (merge-pathnames (uiop:strcat (str:replace-all "/" "-" name) ".json")
                                           *package-embedding-local-directory*))

         (embeddings
           (optimize-embeddings
            (if (probe-file embeddings-file)
                (with-open-file (f embeddings-file)
                  (shasht:read-json f))
                (or (package-symbol-embeddings-try-download package embeddings-file)
                    (package-symbol-embeddings-compute package embeddings-file))))))

    (maphash (lambda (s e)
               (setf (gethash s *symbol-embeddings*) e))
             embeddings)
    t))
