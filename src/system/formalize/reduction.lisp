

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.reduction
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct))
(in-package :flexpr2.system.formalize.reduction)


;; cnf系の式をできる限り簡単にする
;; SAT解くときにも使うと思うから汎用性あるようにしっかり
