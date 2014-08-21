
(in-package :cl-user)
(defpackage :flexpr2.system.formalize.literalize
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
  (:export
    :literalize))
(in-package :flexpr2.system.formalize.literalize)



(defmethod literalize ((lexpr logical-expression)) lexpr)
(defmethod literalize ((lexpr (eql nil))) lexpr)
(defmethod literalize ((lexpr literal)) lexpr)

(defmethod literalize ((lexpr quantifier-logical-expression))
  (with-accessors ((quants quants) (expr expr)) lexpr

    (make-quantifier-logical-expression 
      quants
      (literalize expr))))

(defmethod literalize ((lexpr connected-logical-expression))
  (with-accessors ((operator operator) (left left) (right right)) lexpr
    (if (null right)
      ;; operator が negation
      (typecase left

        (literal
          (make-literal 
            (not (negation left)) (pred left) (terms left)))

        (connected-logical-expression 
          (if (null (right left))
            (literalize (left left))
            (let ((inner-operator (opr (operator left))))
              (make-connected-logical-expression
                (make-operator (opposite-opr inner-operator))
                (literalize 
                  (make-connected-logical-expression 
                    (make-operator +negation+) (left left) nil))
                (literalize 
                  (make-connected-logical-expression 
                    (make-operator +negation+) (right left) nil))))))

        (quantifier-logical-expression 
          (with-accessors ((quants quants) (expr expr)) left
            (let ((head (car quants)))
              (make-quantifier-logical-expression
                (list (make-quantifier (opposite-quantifier (quant head)) (bound head)))
                (literalize 
                  (make-connected-logical-expression
										(make-operator +negation+)
										(make-quantifier-logical-expression (cdr quants) expr)
										nil)))))))
      (make-connected-logical-expression
        operator
        (literalize left)
        (literalize right)))))

