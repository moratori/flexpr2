

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.term.simpl
	(:use :cl
				:flexpr2.system.constant.constant
				:flexpr2.system.base.struct))
(in-package :flexpr2.system.formalize.term.simpl)


;;; ユーザ定義の関数やLispのビルトインの関数を用いて
;;; 定数項を簡単に表すための規則を定義できるようにする
