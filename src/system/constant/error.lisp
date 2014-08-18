

(in-package :cl-user)
(defpackage :flexpr2.system.constant.error
  (:use :cl)
  (:export
    :value
    :initial-value-required-error
    :invalid-operator-error
    :invalid-token-error
    :invalid-quantifier-error))

(in-package :flexpr2.system.constant.error)

(define-condition initial-value-required-error (error)
  ())

(define-condition invalid-operator-error (error)
  ())

(define-condition invalid-token-error (error)
  ((value 
     :initform ""
     :accessor value
     :initarg :value)))

(define-condition invalid-quantifier-error (error)
  ())
