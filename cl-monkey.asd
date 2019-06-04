;;;; cl-monkey.asd

(asdf:defsystem #:cl-monkey
  :description "Describe cl-monkey here"
  :author "Eric Lorenzana"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "cl-monkey")))))
