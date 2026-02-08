(defsystem "docsearch-ollama"
  :author "Shubhamkar Ayare (digikar@proton.me)"
  :description "Use ollama to augment documentation search."
  :depends-on ("alexandria"
               "dexador"
               "docsearch"
               "more-docstrings"
               "shasht"
               "trivial-arguments"
               "uiop")
  :components ((:file "package")
               (:file "utils")
               (:file "embeddings")
               (:file "docsearch-ollama")))
