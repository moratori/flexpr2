

(defpackage flexpr2.asd
	(:use :cl :asdf))

(in-package flexpr2.asd)


(defsystem flexpr2
	:serial t
	:version "1.0"
	:author "Katsuyoshi Ozaki"
	:depends-on (:yacc)
	:components 
	((:module "src"
		:serial t
		:components 
		((:module "system"
			:serial t
			:components 
			((:module "constant"
				:serial t
				:components 
				((:file "error")
				 (:file "constant")))
			 (:module "base"
				:serial t
				:components 
				((:file "util")
				 (:file "struct")
				 (:file "unifier")))
			 (:module "io"
				:serial t
				:components
				((:file "printer")
				 (:file "reader")))
			 (:module "formalize"
				:serial t
				:components 
				((:file "preprocessor")
				 (:file "rmquant")
			   (:file "rmopr")
				 (:file "prenex")
				 (:file "cnf")
				 (:file "skolem")
				 (:file "reduction")))
			 (:module "core"
				:serial t
				:components 
				((:file "glinear")
				 (:file "snl")
				 (:file "rewriting")
 				 (:file "sat"))))) 

			 (:module "interface"
				:serial t
				:components 
				((:file "constant")
				 (:file "repl")))))))
	


