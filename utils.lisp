(in-package :docsearch-ollama)

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
                             (format t "function, it takes the following arguments:~%    ~A"
                                     arglist))
                         (error ()
                           (format nil "~A: " doc-type))))
                      ((eq 'setf doc-type)
                       (handler-case
                           (let ((arglist (trivial-arguments:arglist `(setf ,symbol))))
                             (format t "setf-function, it takes the following arguments:~%    ~A"
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

(defun full-symbol-name (symbol)
  (declare (optimize speed)
           (type symbol symbol))
  (format nil "~A:~A"
          (package-name (symbol-package symbol))
          symbol))

(defun ensure-package-name (package)
  (if (packagep package)
      (package-name package)
      (package-name (find-package package))))

(defun parse-symbol-name (full-symbol-name)
  (declare (type string full-symbol-name))
  (let ((colon-pos (position #\: full-symbol-name)))
    (find-symbol (subseq full-symbol-name (1+ colon-pos))
                 (subseq full-symbol-name 0 colon-pos))))
