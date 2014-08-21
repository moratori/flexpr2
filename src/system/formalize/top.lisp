

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
    :simplify-premises-lexpr
		:simplify-conseq-lexpr)
  )
(in-package :flexpr2.system.formalize.top)



(defun simplify-base (lexpr)
	(literalize 
		(remove-operator 
			(remove-unused-quantifier 
				(remove-domain-sugar lexpr)))))


(defun clause-formation (lexpr)
	lexpr
	)

(defun simplify-premises-lexpr (lexpr)
	(clause-formation 
		(skolemization
			(naive-cnf 
				(prenex
					(simplify-base lexpr))))))


(defun simplify-conseq-lexpr (lexpr)
	;; rule は ((x . U-123) (y . U-456) ...) の形
	;; U-123とかU-456 を追っていけば具体的な項をもとめられ、
	;; かつユーザが入力した時につかった変数に対応させられる
	(multiple-value-bind (expr rule) (prenex (simplify-base lexpr))
		(values 
			(clause-formation
				(skolemization 
					(naive-cnf
						(literalize 
							(make-connected-logical-expression (make-operator +negation+) expr nil)))))
			rule)))







