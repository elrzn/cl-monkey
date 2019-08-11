;;;; parser.lisp

(in-package :cl-monkey)

(defclass parser ()
  ((lexer
    :initarg :lexer
    :accessor parser-lexer
    :type lexer
    :documentation "The LEXER, that will be consumed by the PARSER.")
   (current-token
    :accessor parser-current-token
    :type token
    :documentation
    "Similar to LEXER's #'POSITION, but pointing to the current TOKEN.")
   (peek-token
    :accessor parser-peek-token
    :type token
    :documentation
    "Similar to LEXER's #'READ-POSITION, but pointing to the current TOKEN."))
  (:documentation "Object that, from the given LEXER, will produce an AST."))

(defun make-parser (lexer)
  "From an already constructed LEXER, return a PARSER for it."
  (declare (type lexer lexer))
  (let ((parser (make-instance 'parser :lexer lexer)))
    (dotimes (_ 2)
      (parser-next-token parser))
    parser))

(defmethod parser-next-token ((self parser))
  "Read TOKEN from the LEXER, and shift current token the previously
peeked one."
  (setf (parser-current-token self) (parser-peek-token self))
  (setf (parser-peek-token self) (lexer-next-token (parser-lexer self))))
