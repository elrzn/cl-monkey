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

(defclass identifier (expression)
  ((token
    :initarg :token
    :reader token
    :type token
    :initform nil)
   (value
    :initarg :value
    :reader value
    :type string
    :initform nil)))

(defclass let-statement (statement)
  ((token
    :initarg :token
    :reader token
    :type token-type
    :initform nil)
   (name
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

(defmethod node-token-literal ((self identifier))
  (token-literal (token self)))

(defmethod node-token-literal ((self let-statement))
  (token-literal (token self)))
