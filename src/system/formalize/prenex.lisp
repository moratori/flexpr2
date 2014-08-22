

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.prenex
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:import-from :flexpr2.system.base.term
								:substitute-term
								:update-rule
								:rule=
								:term=)
	(:export
		:prenex
		:re-prenex))
(in-package :flexpr2.system.formalize.prenex)


;; 冠頭形に変形する


;;; literalize された後の論理式を意図しているため
;;; right が nil でアルことは許されない

;;; 束縛変数を正規化する。　これをやらないと冠頭形にできないため
;;; rename-bound-varがトップレベルにあるのは外から置き換え規則old new をいじられないようにするため

;;; ここでどの束縛変数をなにに変えたかを記憶しておかないと後々解を求められなくなるね
;;; 特に存在量化されてた束縛変数はあとあと求められるようにしたほうがいい
;;; ExP(x) & ExQ(x) みたいな時にどうやって x をトレースする?
;;; 述語 P の x と Q　の x という風に分けるしか無い気がする -> 述語に対応付けても無理だ => Ex.P(x) & Ex.(P(x) V Q(x))
;;;
;;; Ex.(P(x) & Ex.Q(x)) みたいな式が入力された時、二箇所のxは別物として扱われてるわけだから
;;; x -> #:G123
;;; x -> #:G234 みたいなふうにしちゃって 左辺の x 同士は違うものだよってしちゃえばいいよね(だってそうなるようなの入力してきたのはユーザなんだから)
;;; だから違うシンボルに置き換えられるんだよって事にすればいい
;;;
;;; rename-bound-var は Eに束縛された変数がどれに置き換えられたかも返す
;;; よって ((x . VAR1) (x . VAR2)) も許す
(defun rename-bound-var (lexpr)
	;; 束縛変数の正規化を行う
	(typecase lexpr
		(literal 
			(values lexpr nil))
		(connected-logical-expression
			(multiple-value-bind (expr1 rule1) (rename-bound-var (left lexpr))
				(multiple-value-bind (expr2 rule2) (rename-bound-var (right lexpr))
					(values 
						(make-connected-logical-expression 
							(operator lexpr)
							expr1
							expr2)
						(append rule1 rule2)))))
		(quantifier-logical-expression
			(rename-bound-var% lexpr nil nil))))



(defmethod rename-bound-var% ((obj term-container) old new)
	(assert (not (or (null old) (null new))))
	(substitute-term obj old new))

(defmethod rename-bound-var% ((lexpr connected-logical-expression) old new)
	(with-accessors ((operator operator) (left left) (right right)) lexpr
		(assert (and (not (null right))
								 (not (or (null old) (null new)))))
		
		(multiple-value-bind (expr1 rule1) (rename-bound-var% left old new)
			(multiple-value-bind (expr2 rule2) (rename-bound-var% right old new)
				(values 
					(make-connected-logical-expression
						operator
						expr1
						expr2)
					(append rule1 rule2))))))

(defmethod rename-bound-var% ((lexpr quantifier-logical-expression) old new)
	(with-accessors ((quants quants) (expr expr)) lexpr
		;; quants と old new から書き換え規則を作る
		(let* (all-rule
					 (rewrite-rule
						 (mapcar 
							(lambda (q)
								(cons (bound q) (make-vterm (gensym +unique-bound-var-prefix+) nil))) quants))
					 (new-expr 
						 (reduce 
							 (lambda (res rule)
								 (multiple-value-bind (tmp-new-expr tmp-rule) (rename-bound-var% res (car rule) (cdr rule))
									 (setf all-rule (nconc all-rule tmp-rule))
									 tmp-new-expr)) 
									 rewrite-rule :initial-value expr)))
			(values 
					(make-quantifier-logical-expression
						(mapcar 
							(lambda (rule q)
								(destructuring-bind (_ . b) rule
									(declare (ignore _))
									(make-quantifier (quant q) b)))
							rewrite-rule
							quants)
						(if (not (or (null old) (null new)))
							(rename-bound-var% new-expr old new)
							new-expr))
					(append all-rule 
									(loop for q in quants
												for rule in rewrite-rule
												if (eq +exists+ (quant q))
												collect rule))))))


(defmethod join ((operator operator) (left logical-expression) (right logical-expression))
	(make-connected-logical-expression operator left right))


(defmethod join ((operator operator) (left logical-expression) (right quantifier-logical-expression)) (join operator right left))
(defmethod join ((operator operator) (left quantifier-logical-expression) (right logical-expression)) 
	(make-quantifier-logical-expression
		(quants left)
		(make-connected-logical-expression operator (expr left) right)))


;;; left と right が operator によってくっついてる時に それらを適切にくっつけ冠頭形にする
(defmethod join ((operator operator) (left quantifier-logical-expression) (right quantifier-logical-expression))
	(with-accessors ((quants-left quants) (expr-left expr)) left
		(with-accessors ((quants-right quants) (expr-right expr)) right
			(let* ((opr (opr operator))
						 (quant-obj-left-head  (car quants-left))
						 (quant-obj-right-head (car quants-right))
						 (quant-left-head  (quant quant-obj-left-head))
						 (quant-right-head (quant quant-obj-right-head)))
				(cond 
					;; Ax.Φ  & Ax.ψ  => Ax.(Φ  & ψ )
					((and (eq quant-left-head quant-right-head)
								(eq quant-left-head +forall+)
								(eq opr +conjunctive+))
					 (let* ((A (make-quantifier-logical-expression  (cdr quants-left)  expr-left))
									(B (substitute-term 
											 (make-quantifier-logical-expression 
												 (cdr quants-right) expr-right) 
											 (bound quant-obj-right-head) 
											 (bound quant-obj-left-head)))
									(new-matrix (prenex% (make-connected-logical-expression operator A B))))
							 (make-quantifier-logical-expression
								 (cons quant-obj-left-head (quants new-matrix))
								 (expr new-matrix))))
					;; Ex.Φ  V Ex.ψ  => Ex.(Φ  V ψ )
					((and (eq quant-left-head quant-right-head)
								(eq quant-left-head +exists+)
								(eq opr +disjunctive+))
					 (let* ((A (make-quantifier-logical-expression  (cdr quants-left)  expr-left))
									(rule (cons (bound quant-obj-right-head) (bound quant-obj-left-head)))
									(B (substitute-term 
											 (make-quantifier-logical-expression (cdr quants-right) expr-right) 
											 (car rule)
											 (cdr rule)))
									(new-matrix (prenex% (make-connected-logical-expression operator A B))))

						 (push rule addtional-rule)

						 (make-quantifier-logical-expression
								 (cons quant-obj-left-head (quants new-matrix))
								 (expr new-matrix))))
					;; Q1x1.Φ  & Q2x2.ψ  => Q1x1 Q2x2. (Φ  & ψ )
					;; Q1x1.Φ  V Q2x2.ψ  => Q1x1 Q2x2. (Φ  V ψ )
					(t 
						(let* ((A (make-quantifier-logical-expression (cdr quants-left)  expr-left ))
									 (B (make-quantifier-logical-expression (cdr quants-right) expr-right))
									 (new-matrix (prenex% (make-connected-logical-expression operator A B))))
								(make-quantifier-logical-expression 
									(append (list quant-obj-left-head quant-obj-right-head) (quants new-matrix))
									(expr new-matrix)))))))))



;; G には x が自由出現するがFにはしないことを意図している
;; なぜなら rename-bound-var されていて x はユニークなはずだから
;; ただし ユニークであるはずの(gensymされる) x を予測して Fに自由変数として入れられた場合があると
;; 束縛変数を不正に増やしてしまうことになるから rename-bound-var してあっても一応自由出現しないことを
;; たしかめてやらねばならない. で必要ならエラー吐こう.
;;
;; Ax.G V F  =>  Ax.(G V F)
;; Ax.G & F  =>  Ax.(F & F)
;; Ex.G V F  =>  Ex.(G V F)
;; Ex.G & F  =>  Ex.(G & F)
;;

(defun prenex (lexpr)
	(multiple-value-bind (expr rule) (rename-bound-var lexpr)	
		;; ここで一応の rule をとっておく
		;; といっても大部分はこのrule のだけを返せばおｋなはずなんだけど
		;; prenex form にするときに Ex.P(x) V Ey.Q(y)
		;; みたいのがきたときには Ex.(P(x) V Q(y)) にできるから
		;; この場合に y -> x みたいな規則ができる. でこの規則を rule に適用させてやる
		(let (addtional-rule)
			(declare (special addtional-rule))
			(let ((result (prenex% expr)))
				(values 
					result
					(remove-duplicates 
						(update-rule rule addtional-rule) 
						:test #'rule=))))))

(defmethod prenex% ((lexpr literal)) lexpr)

(defmethod prenex% ((lexpr connected-logical-expression))
	(join (operator lexpr) (prenex% (left lexpr)) (prenex% (right lexpr))))

(defmethod prenex% ((lexpr quantifier-logical-expression))
	(with-accessors ((quants quants) (expr expr)) lexpr
		(multiple-value-bind (new-expr rule) (prenex% expr)
			(if (typep new-expr 'quantifier-logical-expression)
				(with-accessors ((new-quants quants) (new-matrix expr)) new-expr
					(values
						(make-quantifier-logical-expression
							(append quants new-quants) new-matrix)
						rule))
				(make-quantifier-logical-expression
					quants new-expr)))))


;;; 一度 prenex した式を negation つけて literalize しても
;;; 量化子の場所は変わらず冠頭形なんだけど
;;; Ax.(Ay.(Az.Ex.(Ew.~P))) みたいになっちゃうのですこしだけ綺麗にする
(defmethod re-prenex ((lexpr literal)) lexpr)
(defmethod re-prenex ((lexpr connected-logical-expression)) lexpr)
(defmethod re-prenex ((lexpr quantifier-logical-expression))
	(with-accessors ((quants quants) (expr expr)) lexpr
		(let ((tmp (re-prenex expr)))
			(make-quantifier-logical-expression
				(append quants (quants tmp)) (expr tmp)))))

