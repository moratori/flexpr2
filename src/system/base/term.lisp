

(in-package :cl-user)
(defpackage :flexpr2.system.base.term
  (:use :cl
        :flexpr2.system.constant.constant
        :flexpr2.system.base.struct)
  (:export
    :free-occurrence?
		:substitute-term
    :term=)
  )
(in-package :flexpr2.system.base.term)


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
(defmethod substitute-term ((target vterm) (old vterm) (new vterm))
	(if (term= target old) new target))
(defmethod substitute-term ((target fterm) (old vterm) (new vterm))
	(make-fterm
		(fsymbol target)
		(mapcar 
			(lambda (x)
				(substitute-term x old new))
			(terms target))))

(defmethod substitute-term ((target literal) (old vterm) (new vterm))
	(make-literal
		(negation target)
		(pred target)
		(mapcar 
			(lambda (x)
				(substitute-term x old new))
			(terms target))))

(defmethod substitute-term ((target connected-logical-expression) (old vterm) (new vterm))
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



