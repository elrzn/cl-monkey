;;;; token.lisp

(in-package #:cl-monkey)

(deftype token-type (&optional type)
  `(string ,type))

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
