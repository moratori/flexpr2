

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.cnf
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:export
		:naive-cnf)
	)
(in-package :flexpr2.system.formalize.cnf)


;; 連言標準形にする
;; SAT解くときにも使うと思うから汎用性あるように実装



(defun naive-cnf (lexpr)
	lexpr)
