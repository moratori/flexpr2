
(ql:quickload :lisp-unit)
(ql:quickload :flexpr2)

(use-package :lisp-unit)
(import '(flexpr2.system.io.reader:parse
          flexpr2.system.formalize.lexpr.top:simplify-premises-lexpr
					flexpr2.system.formalize.lexpr.top:simplify-conseq-lexpr-for-resolution))

(defvar *test-case*
  '(
		"Ex.P(x) V Ey.Q(y) V Ez.R(z)"
		"(P(C) V Ex.Q(x)) V (Ey.R(y) V S(C))"
		"Ax.P(x) V Ay.Q(y)"
		"Ax.P(x) & Ax.Q(x)"
		"Ax.P(x) & Ay.Q(y)"
		"Ex.P(x) & Ex.Q(x)"
		"Ex.P(x) V Ex.Q(x)"
		"A(x : R)E(y : H).(P(x) & Q(y) > Ex.R(x,y))"
		"Ax.(P(x) > (Q(x) & Ex.R(x)))"
		"Ex.P(x)"
		"AxEyAwEz.P(x,y,z,w)"


     )
  )

(define-test parser
  (dolist (each *test-case*)
    (let* ((lexpr (parse each)))
			(multiple-value-bind (expr rule) (simplify-premises-lexpr lexpr)
				(format t "EXPR: ~A~%RULE: ~A~%~%" expr rule)
				(assert-true expr)
				)
			
      )))


(let ((result (run-tests '(parser))))
  (print-errors result)
  (print-failures result))


