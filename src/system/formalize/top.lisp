

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.top
  (:use :cl
        :flexpr2.system.constant.constant
        :flexpr2.system.base.struct
        :flexpr2.system.formalize.preprocessor
        :flexpr2.system.formalize.rmquant
        :flexpr2.system.formalize.rmopr
        :flexpr2.system.formalize.literalize
        :flexpr2.system.formalize.prenex
        :flexpr2.system.formalize.cnf
        :flexpr2.system.formalize.skolem
        :flexpr2.system.formalize.reduction
        )
  (:export
    :simplify)
  )
(in-package :flexpr2.system.formalize.top)




(defmethod simplify ((lexpr logical-expression))
  (literalize 
    (remove-operator 
    (remove-unused-quantifier 
      (remove-domain-sugar lexpr))))
  )
