

(in-package :cl-user)
(defpackage :flexpr2.system.formalize.prenex
  (:use 
    :cl
    :flexpr2.system.constant.constant
    :flexpr2.system.base.struct)
	(:import-from :flexpr2.system.base.term
								:term=)
	(:export
		:rename-bound-var))
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

(defmethod rename-bound-var% ((obj vterm) old new)
	(assert (not (or (null old) (null new))))
	;; values が無いのに bind が待ち構えてる時は nil に束縛されるのでわざわざ nil を valuesする必要はない

	(if (term= obj old) new obj))

(defmethod rename-bound-var% ((obj fterm) old new)
	(assert (not (or (null old) (null new))))
	;; values が無いのに bind が待ち構えてる時は nil に束縛されるのでわざわざ nil を valuesする必要はない

	(make-fterm 
		(fsymbol obj)
		(mapcar 
			(lambda (x) (rename-bound-var% x old new)) 
			(terms obj))))

(defmethod rename-bound-var% ((lexpr literal) old new)
	(assert (not (or (null old) (null new))))
	;; values が無いのに bind が待ち構えてる時は nil に束縛されるのでわざわざ nil を valuesする必要はない
	(make-literal
		(negation lexpr)
		(pred lexpr)
		(mapcar 
			(lambda (term) 
				(rename-bound-var% term old new))
			(terms lexpr))))

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
					(append rule1 rule2)
					)
				)
			)))

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




