;;;; lexer-test.lisp

(in-package #:cl-monkey-test)

(subtest "Test next token"
  (let* ((input "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);

!-/5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 10;
")
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
                  (cl-monkey::+token-left-parenthesis+ . "(")
                  (cl-monkey::+token-ident+ . "x")
                  (cl-monkey::+token-comma+ . ",")
                  (cl-monkey::+token-ident+ . "y")
                  (cl-monkey::+token-right-parenthesis+ . ")")
                  (cl-monkey::+token-left-brace+ . "{")
                  (cl-monkey::+token-ident+ . "x")
                  (cl-monkey::+token-plus+ . "+")
                  (cl-monkey::+token-ident+ . "y")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-right-brace+ . "}")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-let+ . "let")
                  (cl-monkey::+token-ident+ . "result")
                  (cl-monkey::+token-assign+ . "=")
                  (cl-monkey::+token-ident+ . "add")
                  (cl-monkey::+token-left-parenthesis+ . "(")
                  (cl-monkey::+token-ident+ . "five")
                  (cl-monkey::+token-comma+ . ",")
                  (cl-monkey::+token-ident+ . "ten")
                  (cl-monkey::+token-right-parenthesis+ . ")")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-bang+ . "!")
                  (cl-monkey::+token-minus+ . "-")
                  (cl-monkey::+token-slash+ . "/")
                  (cl-monkey::+token-int+ . "5")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-int+ . "5")
                  (cl-monkey::+token-less-than+ . "<")
                  (cl-monkey::+token-int+ . "10")
                  (cl-monkey::+token-greater-than+ . ">")
                  (cl-monkey::+token-int+ . "5")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-if+ . "if")
                  (cl-monkey::+token-left-parenthesis+ . "(")
                  (cl-monkey::+token-int+ . "5")
                  (cl-monkey::+token-less-than+ . "<")
                  (cl-monkey::+token-int+ . "10")
                  (cl-monkey::+token-right-parenthesis+ . ")")
                  (cl-monkey::+token-left-brace+ . "{")
                  (cl-monkey::+token-return+ . "return")
                  (cl-monkey::+token-true+ . "true")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-right-brace+ . "}")
                  (cl-monkey::+token-else+ . "else")
                  (cl-monkey::+token-left-brace+ . "{")
                  (cl-monkey::+token-return+ . "return")
                  (cl-monkey::+token-false+ . "false")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-right-brace+ . "}")
                  (cl-monkey::+token-int+ . "10")
                  (cl-monkey::+token-equals+ . "==")
                  (cl-monkey::+token-int+ . "10")
                  (cl-monkey::+token-semicolon+ . ";")
                  (cl-monkey::+token-int+ . "10")
                  (cl-monkey::+token-not-equals+ . "!=")
                  (cl-monkey::+token-int+ . "10")
                  (cl-monkey::+token-semicolon+ . ";")))
         (lexer (cl-monkey::make-lexer input)))
    (dolist (test tests)
      (let* ((token (cl-monkey::lexer-next-token lexer))
             (expected-type (eval (car test)))
             (expected-literal (cdr test)))
        (is (cl-monkey::token-type token) expected-type)
        (is (cl-monkey::token-literal token) expected-literal)))))
