
(ql:quickload :lisp-unit)
(ql:quickload :flexpr2)

(use-package :lisp-unit)

(import '(flexpr2.system.formalize.lexpr.top:simplify-premises-lexpr
					flexpr2.system.io.reader:parse))


(defvar *test-data*
	'(
		"P > Q V S & U > T"
		"S > U & P V R > Q"
		"P"
		"~P"
		"P > ~P"
		"(P & Q) V (R V S & U)"
		"(p - q) & (s - u)"
		"(P & Q) V (R & S)"
		"(P & Q) V (R V K)"
		"(P V Q) V (R & S)"


		"A(x : R).(P(x) V Q)"
		"AxEy.((P(x,y) & Ex.Q(x)) V (AzAw.(R(z,w) V K(x))))"

		)
	)


(dolist (each *test-data*)
	(format t "~%~A~%" (simplify-premises-lexpr (parse each)))
	)
