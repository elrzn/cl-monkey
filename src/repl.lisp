;;;; repl.lisp

(in-package :cl-monkey)

(defparameter +prompt+ ">> ")

(defun repl-start ()
  (loop for lexer = (make-lexer (read-line))
        do (loop for token = (lexer-next-token lexer)
                 while (not (equalp (token-type token) +token-eof+))
                 do (format t "~a~%" token))))
