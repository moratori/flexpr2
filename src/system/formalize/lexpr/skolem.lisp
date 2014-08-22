

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.lexpr.skolem
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:import-from :flexpr2.system.base.termutil
		:substitute-term)
	(:export
		:skolemization)
	)
(in-package :flexpr2.system.formalize.lexpr.skolem)


;;; 量化子を削除して全称量化な式だけに変形する
;;; 当然量化部分が全てなくなる場合もある

;;; skolemization は prenex normal フォームな式を受け取ることを意図している

(defmethod skolemization((lexpr logical-expression) rmquant) lexpr)

;;; AxEyAzEwEv みたいなのをとって
;;; y -> f(x)
;;; w -> g(x,z)
;;; v -> h(x,z)みたいのを返す
(defun new-quants-part (quants rmquant)
	(labels
		((make-function (lst)
				(make-fterm
					(gensym +skolem-function-prefix+) lst)) 
		 (make-rule ()
				(loop for tq in quants
					    for i from 0
					    if (eq rmquant (quant tq))
					    collect 
					    (let ((lst 
									(loop with res = nil
												finally (return (reverse res))
												for j from 0 below i
												for q in quants
												if (not (eq rmquant (quant q)))
												do (push (bound q) res))))
								(cons 
									(bound tq)
									(if (null lst) 
										(make-vterm (gensym +skolem-constant-prefix+) t)
										(make-function lst)))))))
		(values
			(remove-if 
				(lambda (q) (eq rmquant (quant q))) quants)
			(make-rule))))


(defmethod skolemization ((lexpr quantifier-logical-expression) rmquant)
	(with-accessors ((quants quants) (expr expr)) lexpr
		(multiple-value-bind (new-quants rule) (new-quants-part quants rmquant)
			(make-quantifier-logical-expression
				new-quants
				(reduce 
					(lambda (result rule)
						(substitute-term result (car rule) (cdr rule)))
					rule
					:initial-value expr)))))


