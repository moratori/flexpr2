

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.lexpr.top
  (:use :cl
        :flexpr2.system.constant.constant
        :flexpr2.system.base.struct
        :flexpr2.system.formalize.lexpr.preprocessor
        :flexpr2.system.formalize.lexpr.rmquant
        :flexpr2.system.formalize.lexpr.rmopr
        :flexpr2.system.formalize.lexpr.literalize
        :flexpr2.system.formalize.lexpr.prenex
        :flexpr2.system.formalize.lexpr.cnf
        :flexpr2.system.formalize.lexpr.skolem
        :flexpr2.system.formalize.lexpr.simpl
        )
	(:import-from :flexpr2.system.base.termutil
								:closed-lexpr?)
  (:export
    :simplify-premises-lexpr
		:simplify-conseq-lexpr-for-resolution)
  )
(in-package :flexpr2.system.formalize.lexpr.top)



(defun simplify-base (lexpr)
	(literalize 
		(remove-operator 
			(remove-unused-quantifier 
				(remove-domain-sugar lexpr)))))


(defun clause-formation (lexpr)
	lexpr
	)


(defun simplify-premises-lexpr (lexpr &optional (not-closed-error nil))
	(let ((tmp  (skolemization (prenex (simplify-base lexpr)) +exists+)))

		(when (and not-closed-error (not (closed-lexpr? tmp)))
			(error (make-condition 'free-variable-error :value tmp)))

		(clause-formation (naive-cnf tmp))))



(defun simplify-conseq-lexpr-for-resolution (lexpr &optional (not-closed-error nil))
	;; rule は ((x . U-123) (y . U-456) ...) の形
	;; U-123とかU-456 を追っていけば具体的な項をもとめられ、
	;; かつユーザが入力した時につかった変数に対応させられる
	;;
	;;;; 一度 prenex にして negation しても確かに以前として冠頭形ではあるんだけどすこし汚れるので re-prenexする
	(multiple-value-bind (expr rule) (prenex (simplify-base lexpr))
		(let ((tmp (skolemization (re-prenex (literalize (make-connected-logical-expression (make-operator +negation+) expr nil))) +exists+)))

			(when (and not-closed-error (not (closed-lexpr? tmp)))
				(error (make-condition 'free-variable-error :value tmp)))
			
			(values (clause-formation (naive-cnf tmp)) rule))))




