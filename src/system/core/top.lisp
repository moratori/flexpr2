

(in-package :cl-user)
(defpackage :flexpr2.system.core.top
  (:use 
    :cl
		:flexpr2.system.constant.constant
		:flexpr2.system.base.struct
		:flexpr2.system.formalize.lexpr
		)
	(:export
		:prove))

(in-package :flexpr2.system.core.top)



;;; premises は論理式のリスト


(defun judge (premises conseq) (assert (and  (every (lambda (x) (typep x 'logical-expression)) premises) (typep conseq 'logical-expression)) )
	(let ((premises-clause-form (mapcan #'simplify-premises-lexpr premises))
				(conseq-clause-form   (simplify-premises-lexpr conseq))
				(nconseq-clause-form  (simplify-conseq-lexpr-for-resolution conseq)))

		(cond 
			(())
			(t 
				
				)
			)

		)
	) 



(defun prove (premises conseq) (assert (and  (every (lambda (x) (typep x 'logical-expression)) premises) (typep conseq 'logical-expression)) )
	(multiple-value-bind (func n-premises n-conseq) (judge premises conseq)
		(funcall func n-premises n-conseq)))
