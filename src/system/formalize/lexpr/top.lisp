

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



;;; clause-form とは clauseオブジェクトのリストなのでそれに変換する
(defun clause-formation (lexpr)
	(labels 
		((collect-literal  (lexpr) 
			 (if (typep lexpr 'literal) (list lexpr)
				 (append (collect-literal (left lexpr)) (collect-literal (right lexpr)))))
		 (clause-form (lexpr)
			 (cond 
				 ((typep lexpr 'literal)
					(list (make-clause (list lexpr))))
				 (t 
					 (if (eq (opr (operator lexpr)) +disjunctive+)
						 (list (make-clause (collect-literal lexpr)))
						 (append 
							 (clause-form (left lexpr))
							 (clause-form (right lexpr))))))))
		(clause-form lexpr)))


;;; skolemization された式は全称量化子しかついてないから
;;; それらは全部取り除く
(defmethod remove-forall ((lexpr logical-expression)) lexpr)
(defmethod remove-forall ((lexpr quantifier-logical-expression))
	(expr lexpr))





(defun simplify-premises-lexpr (lexpr &optional (not-closed-error nil))
	(let ((tmp  (skolemization (prenex (simplify-base lexpr)) +exists+)))

		(when (and not-closed-error (not (closed-lexpr? tmp)))
			(error (make-condition 'free-variable-error :value tmp)))

		(clause-formation 
			(print (naive-cnf (remove-forall tmp))))))



(defun simplify-conseq-lexpr-for-resolution (lexpr &optional (not-closed-error nil))
	;; rule は ((x . U-123) (y . U-456) ...) の形
	;; U-123とかU-456 を追っていけば具体的な項をもとめられ、
	;; かつユーザが入力した時につかった変数に対応させられる
	;;
	;; 式: Ex.P(x) & Ex.Q(x) みたいな式が入力された場合 ((x . U123) (x . U234))
	;; みたいなのが返ってくるのは注意. これ不具合とかではなくて式中の x の出現は２回あるけどどちらも別の文脈で束縛されていて
	;; べつのものだから、 具体的な項は ２つ帰ってくるわけだけど　それなのに 式全体でみた場合 x ひとつしかないから(!!)
	;;
	;;;; 一度 prenex にして negation しても確かに以前として冠頭形ではあるんだけどすこし汚れるので re-prenexする
	(multiple-value-bind (expr rule) (prenex (simplify-base lexpr))
		(let ((tmp (skolemization (re-prenex (literalize (make-connected-logical-expression (make-operator +negation+) expr nil))) +exists+)))

			(when (and not-closed-error (not (closed-lexpr? tmp)))
				(error (make-condition 'free-variable-error :value tmp)))
			
			(values (clause-formation (naive-cnf (remove-forall tmp))) rule))))




