(in-package :cl-monkey)

(defclass node () ())

(defclass statement (node) ())

(defclass expression (node) ())

(defclass program ()
  ((statements
    :initarg :statements
    :accessor program-statements
    :type (cons statement)
    :initform '())))

(defclass token-owner ()
  ((token
    :initarg :token
    :reader token
    :type token
    :initform nil)))

(defclass identifier (expression token-owner)
  ((value
    :initarg :value
    :reader value
    :type string
    :initform nil)))

(defclass let-statement (statement token-owner)
  ((name
    :initarg :name
    :accessor name
    :type identifier
    :initform nil)
   (value
    :initarg :value
    :reader value
    :type expression
    :initform nil)))

(defmacro if-let (bindings then else)
  (let ((symbols (mapcar #'first bindings)))
    `(let ,bindings
       (if (and ,@symbols)
         ,then
         ,else))))

(defgeneric node-token-literal (node-kind))

(defmethod node-token-literal ((self program))
  (if-let ((statement (car (program-statements self))))
      (node-token-literal statement)
    ""))

(defmethod node-token-literal ((self token-owner))
  (token-literal (token self)))
