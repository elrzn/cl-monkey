;;;; lexer-test.lisp

(in-package #:cl-monkey-test)

(subtest "Test next token"
  (let* ((input "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);")
         (tests '((cl-monkey::+token-let+ . "let")
                  (cl-monkey::+token-ident+ . "five")
                  (cl-monkey::+token-assign+ . "=")
                  (cl-monkey::+token-int+ . "5")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-let+ . "let")
                  (cl-monkey::+token-ident+ . "ten")
                  (cl-monkey::+token-assign+ . "=")
                  (cl-monkey::+token-int+ . "10")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-let+ . "let")
                  (cl-monkey::+token-ident+ . "add")
                  (cl-monkey::+token-assign+ . "=")
                  (cl-monkey::+token-function+ . "fn")
                  (cl-monkey::+token-lparen+ . "(")
                  (cl-monkey::+token-ident+ . "x")
                  (cl-monkey::+token-comma+ . ",")
                  (cl-monkey::+token-ident+ . "y")
                  (cl-monkey::+token-rparen+ . ")")
                  (cl-monkey::+token-lbrace+ . "{")
                  (cl-monkey::+token-ident+ . "x")
                  (cl-monkey::+token-plus+ . "+")
                  (cl-monkey::+token-ident+ . "y")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-rbrace+ . "}")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-let+ . "let")
                  (cl-monkey::+token-ident+ . "result")
                  (cl-monkey::+token-assign+ . "=")
                  (cl-monkey::+token-ident+ . "add")
                  (cl-monkey::+token-lparen+ . "(")
                  (cl-monkey::+token-ident+ . "five")
                  (cl-monkey::+token-comma+ . ",")
                  (cl-monkey::+token-ident+ . "ten")
                  (cl-monkey::+token-rparen+ . ")")
                  (cl-monkey::+token-semicolon+ . ";")))
         (lexer (cl-monkey::make-lexer input)))
    (dolist (test tests)
      (let* ((token (cl-monkey::lexer-next-token lexer))
             (expected-type (eval (car test)))
             (expected-literal (cdr test)))
        (is (cl-monkey::token-type token) expected-type)
        (is (cl-monkey::token-literal token) expected-literal)))))
