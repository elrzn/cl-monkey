;;;; lexer-test.lisp

(in-package #:cl-monkey-test)

(subtest "Test next token"
  (let* ((input "=+(){},;")
         (tests '((cl-monkey::+token-assign+    . "=")
                  (cl-monkey::+token-plus+      . "+")
                  (cl-monkey::+token-lparen+    . "(")
                  (cl-monkey::+token-rparen+    . ")")
                  (cl-monkey::+token-lbrace+    . "{")
                  (cl-monkey::+token-rbrace+    . "}")
                  (cl-monkey::+token-comma+     . ",")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-eof+       . "")))
         (lexer (cl-monkey::make-lexer input)))
    (dolist (test tests)
      (let* ((token (cl-monkey::lexer-next-token lexer))
             (expected-type (eval (car test)))
             (expected-literal (cdr test)))
        (is (cl-monkey::token-type token) expected-type)
        (is (cl-monkey::token-literal token) expected-literal)))))
