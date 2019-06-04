;;;; lexer.lisp

(in-package #:cl-monkey)

(defclass lexer ()
  ((input
    :initarg :input
    :reader lexer-input
    :type string
    :initform ""
    :documentation "The input to be consumed by the lexer.")
   (position
    :initarg :position
    :accessor lexer-position
    :type integer
    :initform 0
    :documentation "Current position in input (points to current character).")
   (read-position
    :initarg :read-position
    :accessor lexer-read-position
    :type integer
    :initform 0
    :documentation "Current reading position in input (after current character).")
   (character
    :initarg :character
    :accessor lexer-character
    :type char-code
    :initform ""
    :documentation "Current character under examination.")))

(defmethod lexer-out-of-bounds-p ((lexer lexer))
  "Check whether we have exceeded reading the lexer's input."
  (>= (lexer-read-position lexer)
      (length (lexer-input lexer))))

(defmethod lexer-read-character ((lexer lexer))
  "Consume the lexer's input, reading one character at a time."
  (setf (lexer-character lexer)
        (if (lexer-out-of-bounds-p lexer)
            0
            (char (lexer-input lexer) (lexer-read-position lexer))))
  (setf (lexer-position lexer) (lexer-read-position lexer))
  (incf (lexer-read-position lexer)))

(defun make-lexer (input)
  "Create an instance of a lexer for the given input."
  (declare (type (string input)))
  (let ((l (make-instance 'lexer :input input)))
    (lexer-read-character l)
    l))

(defmethod lexer-next-token ((lexer lexer))
  "Read and retrieve lexer's next token."
  (let ((token (with-slots (character) lexer
                 (case character
                   (#\= (make-token :type +token-assign+ :literal "="))
                   (#\; (make-token :type +token-semicolon+ :literal ";"))
                   (#\( (make-token :type +token-lparen+ :literal "("))
                   (#\) (make-token :type +token-rparen+ :literal ")"))
                   (#\, (make-token :type +token-comma+ :literal ","))
                   (#\{ (make-token :type +token-lbrace+ :literal "{"))
                   (#\} (make-token :type +token-rbrace+ :literal "}"))
                   (#\+ (make-token :type +token-plus+ :literal "+"))
                   (0 (make-token :type +token-eof+ :literal ""))))))
    (lexer-read-character lexer)
    token))
