

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.preprocessor
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct))
(in-package :flexpr2.system.formalize.preprocessor)


;;; シンタックスシュガーを取り除くための処理を行う


(defmethod remove-domain-sugar ((lexpr quantifier-logical-expression))

  )

(defmethod remove-domain-sugar ((lexpr connected-logical-expression))

  )

(defmethod remove-domain-sugar ((lexpr logical-expression))
  lexpr
  )



