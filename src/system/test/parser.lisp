
(ql:quickload :lisp-unit)
(ql:quickload :flexpr2)

(use-package :lisp-unit)
(import '(flexpr2.system.io.reader:parse
          flexpr2.system.formalize.top:simplify))

(defvar *test-case*
  '(
		"Ex.(P(z) & Ex.Q(x))"
		"P V Q"
    "P > Q > R   > S"
    "(P > Q) V S"
    "P"
    "QVS"
    "Q&S"
    "~(P V B)"
    "P > R V S > T & U"
    "~P & R V ~S - S"
    "Ax.P(x)"
    "Ex.P(x)"
    "Ax.(P(x) > Q(x))"
    "Ax.(P(x) & Ey  Az.(P(y,z) > Q(z )   VR(x  , y)))"
    "A(x : R)E(y : D).~R(x,y)"   
    "A(x : D)E(y : D).(P(x,y) > Ez.Q(y))"
    "AxA(y : D1)E(z : D2)AwEa.P(x,z)"
    "~AxEyAz.P(x,y,z)"
    "~Ax.~Ey.P(x,y)"
    "Ax.(P(z) & Ax.Q(x))"
    "~Ax.~P(x)"
    )
  )

(define-test parser
  (dolist (each *test-case*)
    (let ((tmp (simplify (parse each))))
      (assert-true tmp))))


(let ((result (run-tests '(parser))))
  (print-errors result)
  (print-failures result))


