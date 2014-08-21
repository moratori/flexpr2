
(ql:quickload :lisp-unit)
(ql:quickload :flexpr2)

(use-package :lisp-unit)
(import '(flexpr2.system.io.reader:parse
          flexpr2.system.formalize.top:simplify-premises-lexpr
					flexpr2.system.formalize.top:simplify-conseq-lexpr))

(defvar *test-case*
  '(
		"~~Ax.~Ey.P(x,y)"

		#|
		"Ax.P(x) V Ay.Q(y)"
		"Ax.P(x) & Ax.Q(x)"
		"Ax.P(x) & Ay.Q(y)"
		"Ex.P(x) & Ex.Q(x)"
		"Ex.P(x) V Ex.Q(x)"
		"A(x : R)E(y : H).(P(x) & Q(y) > Ex.R(x,y))"
		"Ax.(P(x) > (Q(x) & Ex.R(x)))"
		|#
     )
  )

(define-test parser
  (dolist (each *test-case*)
    (let* ((lexpr (parse each))
					 (tmp1 (simplify-conseq-lexpr lexpr)))
			(format t "~A~%~%" tmp1)
      (assert-true tmp1))))


(let ((result (run-tests '(parser))))
  (print-errors result)
  (print-failures result))


