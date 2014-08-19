

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.rmopr
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
  (:export
    :remove-operator))
(in-package :flexpr2.system.formalize.rmopr)



;;; 演算子は V & ~ だけにする
;;; その他の演算子をこれらで表す様に変形


(defmethod remove-operator ((lexpr logical-expression)) lexpr)
(defmethod remove-operator ((lexpr (eql nil))) lexpr)

(defmethod remove-operator ((lexpr quantifier-logical-expression))
  (with-accessors ((quants quants) (expr expr)) lexpr
    (make-quantifier-logical-expression 
      quants
      (remove-operator expr)))) 

(defmethod remove-operator ((lexpr connected-logical-expression))
  (with-accessors ((operator operator) (left left) (right right)) lexpr
    (let ((ins (opr operator)))
      (cond 
        ((eq +imply+ ins)
         (make-connected-logical-expression
           (make-operator +disjunctive+)
           (make-connected-logical-expression
             (make-operator +negation+) 
             (remove-operator left) nil)
           (remove-operator right)))

        ((eq +equivalence+ ins)
         (make-connected-logical-expression
           (make-operator +conjunctive+)
           (remove-operator 
             (make-connected-logical-expression
               (make-operator +imply+) left right))
           (remove-operator 
             (make-connected-logical-expression
               (make-operator +imply+) right left))))
        (t 
         (make-connected-logical-expression
           operator
           (remove-operator left)
           (remove-operator right)))))))


