;;;; repl.lisp

(in-package :cl-monkey)

(defun repl-start ()
  (loop for lexer = (make-lexer (progn
                                  (princ ">> ")
                                  (read-line)))
        do (loop for token = (lexer-next-token lexer)
                 while (not (equalp (token-type token) +token-eof+))
                 do (format t "~a~%" token))))
