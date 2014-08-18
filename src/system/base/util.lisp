


(in-package :cl-user)
(defpackage :flexpr2.system.base.util
  (:use :cl)
  (:export 
    :defclass*
    :genuniq))
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



(defun genuniq (&optional (prefix "UNQ"))
  (let ((counter 0))
    (lambda (&optional (flag nil))
      (if flag
	(intern (concatenate 'string prefix (format nil "~A" counter)))
	(setf counter 0)))))


(defun single? (list)
  (= 1 (length list)))






