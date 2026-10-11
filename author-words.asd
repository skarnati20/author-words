;; author-words.asd

(asdf:defsystem #:author-words
  :description "Library for learning uncommon words from an author's words"
  :author "Sai Karnati"
  :license "Apache 2"
  :depends-on (:uiop :cl-pdf :chipz)
  :components ((:file "package")
	       (:file "pdf-parser")
	       (:file "file")
	       (:file "llematization")
	       (:file "author-words")))
