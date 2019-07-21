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

(defmethod lexer-peek-character ((lexer lexer))
  "Peek at the next character from the lexer's input."
  (if (lexer-out-of-bounds-p lexer)
      0
      (char (lexer-input lexer) (lexer-read-position lexer))))

(defmethod lexer-read-character ((lexer lexer))
  "Consume the lexer's input, reading one character at a time."
  (setf (lexer-character lexer) (lexer-peek-character lexer))
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
  (let ((token
          (with-slots (character) lexer
            (flet ((augment-token-with-char (token-type next-char)
                     (when (char= (lexer-peek-character lexer) next-char)
                       (let ((current-character character))
                         (lexer-read-character lexer)
                         (make-token :type token-type
                                     :literal (format nil "~a~a"
                                                      current-character
                                                      (character (lexer-character lexer))))))))
              (case character
                (#\= (or (augment-token-with-char +token-eq+ #\=)
                         (make-token :type +token-assign+ :literal (string character))))
                (#\; (make-token :type +token-semicolon+ :literal ";"))
                (#\( (make-token :type +token-lparen+ :literal "("))
                (#\) (make-token :type +token-rparen+ :literal ")"))
                (#\, (make-token :type +token-comma+ :literal ","))
                (#\{ (make-token :type +token-lbrace+ :literal "{"))
                (#\} (make-token :type +token-rbrace+ :literal "}"))
                (#\+ (make-token :type +token-plus+ :literal "+"))
                (#\- (make-token :type +token-minus+ :literal "-"))
                (#\! (or (augment-token-with-char +token-not-eq+ #\=)
                         (make-token :type +token-assign+ :literal (string character))))
                (#\* (make-token :type +token-asterisk+ :literal "*"))
                (#\/ (make-token :type +token-slash+ :literal "/"))
                (#\< (make-token :type +token-lt+ :literal "<"))
                (#\> (make-token :type +token-gt+ :literal ">"))
                (0 (make-token :type +token-eof+ :literal ""))
                (otherwise (cond
                             ;; Read literals.
                             ((letterp character)
                              (let ((literal (lexer-read-identifier lexer)))
                                (return-from lexer-next-token
                                  (make-token :type (lookup-identifier literal)
                                              :literal literal))))
                             ;; Read integers.
                             ((digit-char-p character)
                              (return-from lexer-next-token
                                (make-token :type +token-int+
                                            :literal (lexer-read-mumber lexer))))
                             ;; Default to illegal token.
                             (t (make-token :type +token-illegal+
                                            :literal (string character))))))))))
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

(defmethod lexer-read-with-character-predicate ((lexer lexer) predicate)
  "Keep consuming characters from the input as long as the predicate applies.
Once it doesn't, return the substring of the input that satisfied such
predicate."
  (let ((position (lexer-position lexer)))
    (loop while (funcall predicate (lexer-character lexer))
          do (lexer-read-character lexer))
    (subseq (lexer-input lexer) position (lexer-position lexer))))

(defmethod lexer-read-identifier ((lexer lexer))
  "If the current position happens to be a letter, keep evaluating in order to
capture the full identifier and return it as a STRING."
  (lexer-read-with-character-predicate lexer #'letterp))

(defmethod lexer-read-mumber ((lexer lexer))
  "If the current position happens to a digit, keep evaluating in order to
capture the full number, and return it."
  (lexer-read-with-character-predicate lexer #'digit-char-p))
