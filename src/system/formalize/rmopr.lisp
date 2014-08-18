

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.cnf
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct))
(in-package :flexpr2.system.formalize.cnf)



;;; 演算子は V & ~ だけにする
;;; その他の演算子をこれらで表す様に変形
