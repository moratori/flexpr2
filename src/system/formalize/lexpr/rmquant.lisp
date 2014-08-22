
(in-package :cl-user)
(defpackage :flexpr2.system.formalize.lexpr.rmquant
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
  (:import-from :flexpr2.system.base.termutil
                :free-occurrence?)
  (:export
    :remove-unused-quantifier
    )
  )
(in-package :flexpr2.system.formalize.lexpr.rmquant)

;;; 束縛されていない不必要な量化子を削除する

(defmethod remove-unused-quantifier ((lexpr logical-expression)) lexpr)
(defmethod remove-unused-quantifier ((lexpr (eql nil))) nil) 

(defmethod remove-unused-quantifier ((lexpr connected-logical-expression))
  (make-connected-logical-expression
    (operator lexpr)
    (remove-unused-quantifier (left lexpr))
    (remove-unused-quantifier (right lexpr))))

(defmethod remove-unused-quantifier ((lexpr quantifier-logical-expression))
  (with-accessors ((quants quants) (expr expr)) lexpr
    (let ((new-quants 
            (remove-if-not (lambda (q) (free-occurrence? (bound q) expr)) quants)))
      (let ((new-expr (remove-unused-quantifier expr)))
        (if (null new-quants) new-expr
          (make-quantifier-logical-expression new-quants new-expr))))))
