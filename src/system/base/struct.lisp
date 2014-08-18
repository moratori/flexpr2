
(in-package :cl-user)
(defpackage :flexpr2.system.base.struct
  (:use :cl
        :flexpr2.system.constant.constant)
  (:import-from :flexpr2.system.constant.error
    :initial-value-required-error)
  (:export
    :operator
    :opr
    :make-operator
    
    :term
    :vterm
    :var
    :const
    :make-vterm

    :fterm
    :fsymbol
    :terms
    :make-fterm

    :logical-expression

    :literal
    :negation
    :pred
    :temrs
    :make-literal

    :atomic
    :make-atomic

    :clause
    :literals
    :make-clause

    :quantifier
    :quant
    :bound
    :make-quantifier
    
    :connected-logical-expression
    :operator
    :left
    :right
    :make-connected-logical-expression

    :quantifier-logical-expression
    :quants
    :expr
    :make-quantifier-logical-expression

    )
  )
(in-package :flexpr2.system.base.struct)


(defmacro defclass* (classname super slots &rest more)
  `(progn 
     (defclass ,classname ,super
       ,slots
       ,@more)
     (defun ,(intern (concatenate 'string "MAKE-" (symbol-name classname))) ,(mapcar #'car slots)
       (make-instance 
         ',classname 
         ,@(mapcan (lambda (x) (list (getf (cdr x) :initarg) (car x))) slots)))))



(defclass term () 
  ())


(defclass logical-expression ()
  ())
 

(defclass* vterm (term)
  ((var   :initform (error (make-condition 'initial-value-required-error))
          :initarg :var
          :accessor var
          :type t)
   (const :initform nil
          :initarg :const
          :accessor const
          :type boolean)))


(defclass* fterm (term)
  ((fsymbol :initform (error (make-condition 'initial-value-required-error))
            :initarg :fsymbol
            :accessor fsymbol
            :type symbol)
   (terms   :initform nil
            :initarg :terms
            :accessor terms
            :type list)))


(defclass* operator ()
  ((opr :initform (error (make-condition 'initial-value-required-error))
        :initarg :opr
        :accessor opr
        :type symbol))) 

(defclass* quantifier ()
  ((quant :initform +forall+
          :initarg :quant
          :accessor quant)
   (bound :initform (error (make-condition 'initial-value-required-error))
          :initarg :bound
          :accessor bound)))

(defclass* connected-logical-expression  (logical-expression)
  ((operator :initform (error (make-condition 'initial-value-required-error))
             :initarg :operator
             :accessor operator)
   (left     :initform (error (make-condition 'initial-value-required-error))
             :initarg :left
             :accessor left
             :type logical-expression)
   (right    :initform nil
             :initarg :right
             :accessor right)))

(defclass* quantifier-logical-expression (logical-expression)
  ((quants   :initform (error (make-condition 'initial-value-required-error))
             :initarg :quants
             :accessor quants
             :type list)
   (expr     :initform (error (make-condition 'initial-value-required-error))
             :initarg :expr
             :accessor expr)))






(defclass* literal (logical-expression)
  ((negation :initform nil
             :initarg :negation
             :accessor negation
             :type boolean)
   (pred     :initform (error (make-condition 'initial-value-required-error))
             :initarg :pred
             :accessor pred
             :type symbol)
   (terms    :initform nil
             :initarg :terms
             :accessor terms
             :type list)))


;; literals の要素は全て literal 型
(defclass* clause (logical-expression)
  ((literals :initform nil
             :initarg :literals
             :accessor literals
             :type list)))
 
