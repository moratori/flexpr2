

(in-package :cl-user)
(defpackage :flexpr2.system.base.termutil
  (:use :cl
        :flexpr2.system.constant.constant
        :flexpr2.system.base.struct)
  (:export
    :free-occurrence?
		:closed-lexpr?
		:substitute-term
		:update-rule
		:rule=
    :term=)
  )
(in-package :flexpr2.system.base.termutil)


;;; mgu 求めたり 項の置換とかいろいろ項に関わること


(defmethod term= (a b) nil)

;; term の型によっては equal を変えた方がいいのでは?
(defmethod term= ((term1 vterm) (term2 vterm))
  (and 
    (equal (var term1) (var term2))
    (eq (const term1) (const term2))))

(defmethod term= ((term1 fterm) (term2 fterm))
  (and 
    (eq (fsymbol term1)
        (fsymbol term2))
    (let ((terms1 (terms term1))
          (terms2 (terms term2)))
      (every #'term= terms1 terms2))))





;;; 置き換え元が純粋に term= で比較して true　となるものについて置き換えをおこなう
(defmethod substitute-term ((target vterm) (old vterm) (new term))
	(if (term= target old) new target))
(defmethod substitute-term ((target fterm) (old vterm) (new term))
	(make-fterm
		(fsymbol target)
		(mapcar 
			(lambda (x)
				(substitute-term x old new))
			(terms target))))

(defmethod substitute-term ((target literal) (old vterm) (new term))
	(make-literal
		(negation target)
		(pred target)
		(mapcar 
			(lambda (x)
				(substitute-term x old new))
			(terms target))))

(defmethod substitute-term ((target connected-logical-expression) (old vterm) (new term))
	(make-connected-logical-expression 
		(operator target)
		(substitute-term (left target) old new)
		(substitute-term (right target) old new)))

(defmethod substitute-term ((target quantifier-logical-expression) (old vterm) (new vterm))
	(make-quantifier-logical-expression
		(mapcar 
			(lambda (q)
				(with-accessors ((quant quant) (bound bound)) q
					(make-quantifier
						quant
						(substitute-term bound old new))))
			(quants target))
		(substitute-term (expr target) old new)))


;; newrule は 書き換え規則 oldrule を更新するためのメタな書き換え規則
;; oldrule : ((x . S1) (y . S2) (z . S3) ...)
;; newrule : ((S1 . S123) (S3 . Z455))
;; みたいなときに ((x . S123) (y . S2) (z . Z455)) みたいにしたものを返す
(defun update-rule (oldrule newrule)
	(reduce 
		(lambda (result-rule eachrule)
			(loop for old in result-rule
						collect 
						(destructuring-bind (a . b) old
							(destructuring-bind (a- . b-) eachrule
								(cons a (if (term= b a-) b- b))))))
		newrule
		:initial-value oldrule))

(defun rule= (r1 r2)
	(and
		(term= (car r1) (car r2))
		(term= (cdr r1) (cdr r2)))) 



;;; term が 自由出現するかどうか
(defmethod free-occurrence? ((term vterm) (obj (eql nil))) nil)
(defmethod free-occurrence? ((term1 vterm) (term2 vterm)) (term= term1 term2))

(defmethod free-occurrence? ((term vterm) (obj term-container))
  (some (lambda (x) (free-occurrence? term x)) (terms obj)))

(defmethod free-occurrence? ((term vterm) (lexpr connected-logical-expression))
  (or (free-occurrence? term (left lexpr))
      (free-occurrence? term (right lexpr))))

(defmethod free-occurrence? ((term vterm) (lexpr quantifier-logical-expression))
  (with-accessors ((quants quants) (expr expr)) lexpr
    (and 
      (every 
        (lambda (quantifier)
          (not (term= term (bound quantifier)))) quants)
      (free-occurrence? term expr))))



(defun closed-lexpr? (lexpr)
	(closed-lexpr?% lexpr nil))

(defmethod closed-lexpr?% ((lexpr (eql nil)) (bounds list)) t)

(defmethod closed-lexpr?% ((term vterm) (bounds list))
	(or (const term) (member term bounds :test #'term=)))

(defmethod closed-lexpr?% ((lexpr term-container) (bounds list))
	(every 
		(lambda (x) 
			(closed-lexpr?% x bounds))
		(terms lexpr)))

(defmethod closed-lexpr?% ((lexpr connected-logical-expression) (bounds list))
	(and 
		(closed-lexpr?% (left lexpr)  bounds)
		(closed-lexpr?% (right lexpr) bounds)))

(defmethod closed-lexpr?% ((lexpr quantifier-logical-expression) (bounds list))
	(closed-lexpr?% 
		(expr lexpr)
		(append bounds (mapcar #'bound (quants lexpr)))))










