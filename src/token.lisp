;;;; token.lisp

(in-package #:cl-monkey)

(deftype token-type (&optional type)
  `(string ,type))

(deftype letter ()
  `(satisfies letterp))

(deftype whitespace ()
  `(satisfies whitespacep))

(defstruct token
  (type "" :type token-type)
  (literal "" :type string))

(defparameter +token-illegal+ "ILLEGAL")
(defparameter +token-eof+ "EOF")

;; Identifiers and literals.
(defparameter +token-ident+ "IDENT")
(defparameter +token-int+ "INT")

;; Operators.
(defparameter +token-assign+ "=")
(defparameter +token-plus+ "+")
(defparameter +token-minus+ "-")
(defparameter +token-bang+ "!")
(defparameter +token-asterisk+ "*")
(defparameter +token-slash+ "/")
(defparameter +token-lt+ "<")
(defparameter +token-gt+ ">")
(defparameter +token-eq+ "==")
(defparameter +token-not-eq+ "!=")

;; Delimeters.
(defparameter +token-comma+ ",")
(defparameter +token-semicolon+ ";")
(defparameter +token-lparen+ "(")
(defparameter +token-rparen+ ")")
(defparameter +token-lbrace+ "{")
(defparameter +token-rbrace+ "}")

;; Keywords.
(defparameter +token-function+ "FUNCTION")
(defparameter +token-let+ "LET")
(defparameter +token-true+ "TRUE")
(defparameter +token-false+ "FALSE")
(defparameter +token-if+ "IF")
(defparameter +token-else+ "ELSE")
(defparameter +token-return+ "RETURN")

(defparameter +keywords+
  (alexandria:plist-hash-table (list "fn" +token-function+
                                     "let" +token-let+
                                     "true" +token-true+
                                     "false" +token-false+
                                     "if" +token-if+
                                     "else" +token-else+
                                     "return" +token-return+)
                               :test 'equal)
  "Lookup table that maps STRING to TOKEN-TYPE.")

(defun lookup-identifier (identifier)
  "Try to lookup a reserved word in the keyword translation table in a attempt
to return its TOKEN-TYPE. If not found, default to a regular identifier."
  (declare (type string identifier))
  (gethash identifier +keywords+ +token-ident+))
