
(in-package :cl-user)
(defpackage :flexpr2.system.io.printer
  (:use :cl
        :flexpr2.system.constant.constant
        :flexpr2.system.base.struct)
  (:import-from :flexpr2.system.constant.error
    :invalid-operator-error
    )
  (:export
    :print-object
    )
  )
(in-package :flexpr2.system.io.printer)



(defmethod print-object ((term vterm) stream)
  (format stream "~A" (var term)))

(defmethod print-object ((term typed-vterm) stream)
  (format stream "(~A:~A)"
	  (print-object (vterm term) nil)
	  (format nil "~A" (vtype term))))

(defmethod print-object ((term fterm) stream)
  (format stream "~A(~{~A~^,~})"
          (fsymbol term)
          (mapcar (lambda (x) (print-object x nil)) 
                  (terms term))))

(defmethod print-object ((literal literal) stream)
  (let ((neg (if (negation literal) 
               +negation-char+ ""))
        (pred (symbol-name (pred literal)))
        (terms (mapcar (lambda (x) (print-object x nil)) 
                  (terms literal))))
    (if terms
      (format stream "~A~A(~{~A~^,~})"
              neg
              pred
              terms)
      (format stream "~A~A" neg pred))))

(defmethod print-object ((operator operator) stream)
  (format stream "~A" 
          (let ((rp (opr operator)))
            (->char rp))))

(defmethod print-object ((quantifier quantifier) stream)
  (format stream "~A~A"
          (->char (quant quantifier))
          (print-object (bound quantifier) nil)))




(defmethod print-object ((expr connected-logical-expression) stream)
  (let ((operator (operator expr))
        (left  (left expr))
        (right (right expr)))
    (if right
      (format stream "~A~A ~A ~A~A"
              +rparen+
              (print-object left nil)
              (print-object operator nil)
              (print-object right nil)
              +lparen+)
      (format stream "~A~A~A~A"
              +rparen+
              (print-object operator nil)
              (print-object left nil)
              +lparen+))))

(defmethod print-object ((expr quantifier-logical-expression) stream)
  (format stream "~A~A~A~A~A~A~A"
          +rparen+
          (reduce (lambda (r e) (concatenate 'string r (print-object e nil))) (quants expr) :initial-value "")
          +delimiter+
          +rparen+
          (print-object (expr expr) nil)
          +lparen+
          +lparen+))
