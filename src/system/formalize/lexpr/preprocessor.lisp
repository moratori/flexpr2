

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.lexpr.preprocessor
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
  (:export
    :remove-domain-sugar)
  )
(in-package :flexpr2.system.formalize.lexpr.preprocessor)




;;; シンタックスシュガーを取り除くための処理を行う
;;; A(x : D).E => Ax.(D(x) > E)
;;; E(x : D).E => Ex.(D(x) & E)
;;; いきなり汚いコードになってしまったけど
;;; 一個変換して再帰するより一気にやってしまってもいいと思うけど
;;; やってることは先頭がtyped-vtermであるか量化子が１つであるかの 4種類の場合わけ。

(defmethod remove-domain-sugar ((lexpr logical-expression)) lexpr)
(defmethod remove-domain-sugar ((lexpr (eql nil))) nil)

(defmethod remove-domain-sugar ((lexpr connected-logical-expression))
  (make-connected-logical-expression
    (operator lexpr)
    (remove-domain-sugar (left lexpr))
    (remove-domain-sugar (right lexpr)))) 

(defmethod remove-domain-sugar ((lexpr quantifier-logical-expression))
  (with-accessors ((quants quants) (expr   expr)) lexpr

    (assert (and (listp quants) (not (null quants))))

    (let ((head (car quants)))
      (with-accessors ((quant quant) (bound bound)) head

	  (if (typep bound 'typed-vterm) 
	    (make-quantifier-logical-expression
	      (list (make-quantifier quant (vterm bound)))
	      (make-connected-logical-expression
					(make-operator (quantifier-domain quant))
					(make-literal 
						nil
						(vtype bound)
						(list (vterm bound)))
					(remove-domain-sugar
						(make-quantifier-logical-expression
							(cdr quants)
							expr))))
			
			(make-quantifier-logical-expression
				(list head)
				(remove-domain-sugar
					(make-quantifier-logical-expression
						(cdr quants)
						expr))))))))
 

