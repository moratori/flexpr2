
(in-package :cl-user)
(defpackage :flexpr2.system.base.struct
  (:use :cl
        :flexpr2.system.constant.constant)

  (:import-from :flexpr2.system.constant.error
    :initial-value-required-error)
  (:import-from :flexpr2.system.base.util
    :defclass*)

  (:export
    :operator
    :opr
    :make-operator
    
    :term
    :vterm
    :var
    :const
    :make-vterm

    :typed-vterm
    :vtype
    :make-typed-vterm

    :fterm
    :fsymbol
    :terms
    :make-fterm

    :logical-expression
    :term-container

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


(defclass term () ())
(defclass logical-expression () ())
(defclass term-container () ())

(defclass* vterm (term)
  ((var   :initform (error (make-condition 'initial-value-required-error))
          :initarg :var
          :accessor var
          :type t)
   (const :initform nil
          :initarg :const
          :accessor const
          :type boolean)))


;; A(x:R).P(x) みたいにつかうためのシンタックスシュガー
;; Ax.(R(x) -> P(x)) に展開されるだけのもの
;; すぐに除去るため、このクラスはリーダによってしか使われない
(defclass* typed-vterm (term)
  ((vterm  :initform (error (make-condition 'initial-value-required-error))
           :initarg :vterm
           :accessor vterm
           :type t)
   (vtype  :initform (error (make-condition 'initial-value-required-error))
          :initarg :vtype
          :accessor vtype
	  :type symbol)))


(defclass* fterm (term term-container)
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




(defclass* literal (logical-expression term-container)
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
 
