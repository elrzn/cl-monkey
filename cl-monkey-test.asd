;;;; cl-monkey-test.asd

(asdf:defsystem #:cl-monkey-test
  :description "Describe cl-monkey here"
  :author "Eric Lorenzana"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:prove)
  :components ((:module "t"
                :components ((:file "package")
                             (:file "cl-monkey-test")))))
