

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.lexpr.cnf
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:export
		:naive-cnf)
	)
(in-package :flexpr2.system.formalize.lexpr.cnf)


;;; 連言標準形にする
;;; SAT解くときにも使うと思うから汎用性あるように実装


;; naive-... については V と & に関する分配規則を再帰的に行なって
;; "普通に" CNF 形にする
(defun naive-cnf (lexpr)
	lexpr)


(defun naive-dnf (lexpr)
	lexpr)
