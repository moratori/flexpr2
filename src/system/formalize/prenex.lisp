

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.prenex
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:export
		:rename-bound-var))
(in-package :flexpr2.system.formalize.prenex)


;; 冠頭形に変形する


;;; literalize された後の論理式を意図しているため
;;; right が nil でアルことは許されない

;;; 束縛変数を正規化する。　これをやらないと冠頭形にできないため
;;; rename-bound-varがトップレベルにあるのは外から置き換え規則old new をいじられないようにするため
(defun rename-bound-var (lexpr)
	(typecase 
		(literal lexpr)
		(connected-logical-expression
			(make-connected-logical-expression 
				(operator lexpr)
				(rename-bound-var (left lexpr))
				(rename-bound-var (right lexpr))))
		(quantifier-logical-expression
			(rename-bound-var% lexpr nil nil))))

(defmethod rename-bound-var% ((obj vterm) old new)
	(assert (not (or (null old) (null new))))
	(if (term= obj old) new obj))

(defmethod rename-bound-var% ((obj fterm) old new)
	(assert (not (or (null old) (null new))))
	(make-fterm 
		(fsymbol obj)
		(mapcar 
			(lambda (x) (rename-bound-var% x old new)) 
			(terms obj))))

(defmethod rename-bound-var% ((lexpr connected-logical-expression) old new)
	(with-accessors ((operator operator) (left left) (right right))
		(assert (and (not (null right))
								 (not (or (null old) (null new)))))
		(make-connected-logical-expression
			operator
			(rename-bound-var% left old new)
			(rename-bound-var% right old new))))

(defmethod rename-bound-var% ((lexpr quantifier-logical-expression) old new)
	(with-accessors ((quants quants) (expr expr)) lexpr
		;; quants と old new から書き換え規則を作る
		)
	)







