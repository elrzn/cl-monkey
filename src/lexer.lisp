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
  (declare (type string input))
  (let ((l (make-instance 'lexer :input input)))
    (lexer-read-character l)
    l))

(defmethod lexer-next-token ((lexer lexer))
  "Read and retrieve lexer's next token."
  (lexer-skip-whitespace lexer)
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
                   (0 (make-token :type +token-eof+ :literal ""))
                   (otherwise (if (letterp character)
                                  (let ((literal (lexer-read-identifier lexer)))
                                    (return-from lexer-next-token
                                      (make-token :type (lookup-identifier literal)
                                                  :literal literal)))
                                  (make-token :type +token-illegal+
                                              :literal (string character))))))))
    (lexer-read-character lexer)
    token))

(defmethod lexer-skip-whitespace ((lexer lexer))
  "Ignore whitespace by reading until a non WHITESPACE character is found."
  (loop while (typep (lexer-character lexer) 'whitespace)
        do (lexer-read-character lexer)))

(defun letterp (c)
  "Checks whether the given character is considered a letter by the lexer."
  (or (alpha-char-p c)
      (char= c #\_ #\! #\?)))

(defun whitespacep (c)
  "Checks whether the given character is considered whitespace by the lexer."
  (or (char= c #\Space)
      (char= c #\Tab)
      (char= c #\Newline)))

(defmethod lexer-read-identifier ((lexer lexer))
  "If the current position happens to be a letter, keep evaluating in order to
capture the full identifier and return it as a STRING."
  (let ((position (lexer-position lexer)))
    (loop while (letterp (lexer-character lexer))
          do (lexer-read-character lexer))
    (subseq (lexer-input lexer) position (lexer-position lexer))))
