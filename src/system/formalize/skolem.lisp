

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.skolem
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:export
		:skolemization)
	)
(in-package :flexpr2.system.formalize.skolem)


;;; 量化子を削除してスコーレム標準形にする


(defun skolemization (lexpr)
	lexpr)
