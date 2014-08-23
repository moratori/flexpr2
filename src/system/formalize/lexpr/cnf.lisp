

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.lexpr.cnf
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:export
		:naive-cnf)
	)
(in-package :flexpr2.system.formalize.lexpr.cnf)


;;; 連言標準形にする
;;; SAT解くときにも使うと思うから汎用性あるように実装


;; naive-... については V と & に関する分配規則を再帰的に行なって
;; "普通に" CNF 形にする


;;; skolemization して 量化子はすべて削除されていることを意図しているから
;;; quantifier-logical-expression については考えない
(defmethod naive-cnf ((lexpr literal)) lexpr)
(defmethod naive-cnf ((lexpr connected-logical-expression))
	(with-accessors ((operator operator) (left left) (right right)) lexpr
		(join (opr operator)  (naive-cnf left) (naive-cnf right))))

(defmethod join (opr (left literal) (right literal))
	(make-connected-logical-expression
		(make-operator opr) left right))

(defmethod join (opr (left logical-expression) (right connected-logical-expression))
	(join opr right left))

(defmethod join (opr (left connected-logical-expression) (right logical-expression))
	(with-accessors ((left-operator operator) (left-left left) (left-right right)) left
		(let ((left-opr (opr left-operator)))
			;; ここの条件式だけが CNF 形にするかを決めてる
			(if (and (eq opr +disjunctive+) (eq left-opr +conjunctive+))
				(make-connected-logical-expression
					(make-operator +conjunctive+)
					(naive-cnf
						(make-connected-logical-expression
							(make-operator +disjunctive+) right left-left))
					(naive-cnf
						(make-connected-logical-expression
							(make-operator +disjunctive+) right left-right)))
				(make-connected-logical-expression 
					 (make-operator opr) left right)))))

