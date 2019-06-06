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

(defparameter +keywords+
  (alexandria:plist-hash-table (list "fn" +token-function+
                                     "let" +token-let+)
                               :test 'equal)
  "Lookup table that maps STRING to TOKEN-TYPE.")

(defun lookup-identifier (identifier)
  "Try to lookup a reserved word in the keyword translation table in a attempt
to return its TOKEN-TYPE. If not found, default to a regular identifier."
  (declare (type string identifier))
  (gethash identifier +keywords+ +token-ident+))
