(defpackage :docsearch
  (:use :cl)
  (:export #:*symbol-doc-types*
           #:*search-function*
           #:docsearch))

(in-package :docsearch)

;; Reference: https://www.lispworks.com/documentation/HyperSpec/Body/f_docume.htm
;; TODO: Integrate this with IN-NOMINE
(defvar *symbol-doc-types* '(compiler-macro function method-combination setf structure type variable))

(defun native-search (search-string docstring)
  (when (search search-string docstring :test #'char-equal)
    docstring))

(defvar *search-function* 'native-search
  "A function-designator whose function takes two arguments:
  1. String to be searched for: This is the user input.
  2. The documentation string of the symbol

It should return one of the following values
  - NIL if the symbol is irrelevant to the search string.
  - T if the symbol is relevant
  - a string that describes why it is relevant")

(defun docsearch (search-term &optional packages (mode :external) (method *search-function*))
  "If PACKAGES is a function, it is a predicate that returns whether to search for a particular package."
  (declare (type (or symbol string) search-term)
           (type (or cons function symbol package) packages)
           (type (member :external :internal) mode))
  (let* ((search-string (etypecase search-term
                          (string search-term)
                          (symbol (symbol-name search-term))))
         (packages (or (cond ((consp packages)
                              packages)
                             ((functionp packages)
                              (remove-if-not packages (list-all-packages)))
                             ((not (null packages))
                              (list packages)))
                       (list-all-packages)))
        (already-looked-up (make-hash-table)))
    (loop :for pkg :in packages
          :for package := (if (packagep pkg)
                              pkg
                              (find-package pkg))
          :for symbols := (let ((all-symbols nil))
                            (ecase mode
                              (:external
                               (do-external-symbols (s package)
                                 (push s all-symbols)))
                              (:internal
                               (do-symbols (s package)
                                 (push s all-symbols))))
                            (nreverse all-symbols))
          :do (dolist (symbol symbols)
                ;; Search in the symbol's name or package
                (unless (gethash symbol already-looked-up)
                  (setf (gethash symbol already-looked-up) t)
                  (let* ((no-doc t)
                         (full-name (format nil "~A:~A"
                                            (package-name (symbol-package symbol))
                                            (symbol-name symbol)))
                         (result (funcall method search-string full-name)))
                    (when result
                      (setf no-doc nil)
                      (terpri)
                      (write-string full-name))
                    ;; Search in its documentation
                    (loop :for doc-type :in *symbol-doc-types*
                          :do
                             (let* ((doc (documentation symbol doc-type))
                                    (result (funcall method search-string doc)))
                               (when result
                                 (when no-doc
                                   (setf no-doc nil)
                                   (terpri)
                                   (write-string full-name))
                                 (terpri)
                                 (format t "  ~A" (string-capitalize doc-type)))
                               (when (stringp result)
                                 (write-string ": ")
                                 (terpri)
                                 (pprint-logical-block (*standard-output* nil
                                                                          :per-line-prefix "    ")
                                   (write-string result))
                                 (terpri))))))))))
