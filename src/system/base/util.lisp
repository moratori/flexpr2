


(in-package :cl-user)
(defpackage :flexpr2.system.base.util
  (:use :cl)
  (:export 
    :defclass*))
(in-package :flexpr2.system.base.util)


(defmacro defclass* (classname super slots &rest more)
  `(progn 
     (defclass ,classname ,super
       ,slots
       ,@more)
     (defun ,(intern (concatenate 'string "MAKE-" (symbol-name classname))) ,(mapcar #'car slots)
       (make-instance 
         ',classname 
         ,@(mapcan (lambda (x) (list (getf (cdr x) :initarg) (car x))) slots)))))







