

(in-package :cl-user)
(defpackage :flexpr2.system.base.term
  (:use :cl
        :flexpr2.system.constant.constant
        :flexpr2.system.base.struct)
  (:export
    :free-occurrence?
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



