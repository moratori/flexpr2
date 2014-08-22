

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.lexpr.simpl
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct))
(in-package :flexpr2.system.formalize.lexpr.simpl)


;; cnf系の式をできる限り簡単にする
;; SAT解くときにも使うと思うから汎用性あるようにしっかり
;;
;; トートロジーや矛盾を除去る
