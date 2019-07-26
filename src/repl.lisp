;;;; repl.lisp

(in-package :cl-monkey)

(defun repl-start ()
  (flet ((repl-read ()
           (princ ">> ")
           (read-line)))
    (loop for lexer = (make-lexer (repl-read))
          do (loop for token = (lexer-next-token lexer)
                   while (not (equalp (token-type token) +token-eof+))
                   do (format t "~a~%" token)))))
