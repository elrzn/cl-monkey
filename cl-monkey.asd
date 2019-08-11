;;;; cl-monkey.asd

(asdf:defsystem #:cl-monkey
  :description "A Common Lisp implementation of the Monkey programming language."
  :author "Eric Lorenzana"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "cl-monkey")
                             (:file "token")
                             (:file "lexer")
                             (:file "repl")
                             (:file "ast")
                             (:file "parser")))))
