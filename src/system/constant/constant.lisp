
(in-package :cl-user)
(defpackage :flexpr2.system.constant.constant
  (:use :cl :flexpr2.system.constant.error)
  (:export 
    :+negation+
    :+negation-char+

    :+imply+
    :+imply-char+

    :+equivalence+
    :+equivalence-char+

    :+disjunctive+
    :+disjunctive-char+

    :+conjunctive+
    :+conjunctive-char+

    :+lparen+
    :+rparen+
    :+separater+
    :+constant-prefix+

    :+operators-char+
    :+binary-operators-char+
    :+monadic-operators-char+
    :+delimiter+

    :+forall+
    :+forall-char+
    :+exists+
    :+exists-char+
    :+quantifiers-char+
    :+reserved+

    :->char
    :char->

    )
  )

(in-package :flexpr2.system.constant.constant)


(defvar +lparen+ #\))
(defvar +rparen+ #\()
(defvar +delimiter+ #\.)
(defvar +separater+ #\,)

(defvar +negation-char+    #\~)
(defvar +imply-char+       #\>)
(defvar +equivalence-char+ #\-)
(defvar +disjunctive-char+ #\V)
(defvar +conjunctive-char+ #\&)
(defvar +forall-char+ #\A)
(defvar +exists-char+ #\E)

(defvar +reserved+ 
  (list 
    +lparen+
    +rparen+
    +delimiter+
    +separater+
    +negation-char+
    +imply-char+
    +equivalence-char+
    +disjunctive-char+
    +conjunctive-char+
    +forall-char+
    +exists-char+))

(defvar +constant-prefix+ #\_)

(defvar +forall+      'forall)
(defvar +exists+      'exists)
(defvar +negation+    'negation)
(defvar +imply+       'imply)
(defvar +equivalence+ 'equivalence)
(defvar +disjunctive+ 'disjunctive)
(defvar +conjunctive+ 'conjunctive)


(defun ->char (obj)
  (cond 
    ((eq +forall+ obj)      +forall-char+)
    ((eq +exists+ obj)      +exists-char+)
    ((eq +negation+ obj)    +negation-char+)
    ((eq +imply+ obj)       +imply-char+)
    ((eq +equivalence+ obj) +equivalence-char+)
    ((eq +disjunctive+ obj) +disjunctive-char+)
    ((eq +conjunctive+ obj) +conjunctive-char+)
    (otherwise 
      (error (make-condition 'invalid-operator-error :value obj)))))

(defun char-> (char)
  (cond 
    ((char= +forall-char+ char)      +forall+)
    ((char= +exists-char+ char)      +exists+)
    ((char= +negation-char+ char)    +negation+)
    ((char= +imply-char+ char)       +imply+)
    ((char= +equivalence-char+ char) +equivalence+)
    ((char= +disjunctive-char+ char) +disjunctive+)
    ((char= +conjunctive-char+ char) +conjunctive+)
    (otherwise 
      (error (make-condition 'invalid-operator-error :value obj)))))

(defvar +operators-char+ 
  (list +negation-char+
        +imply-char+
        +equivalence-char+
        +disjunctive-char+
        +conjunctive-char+))

(defvar +monadic-operators-char+
  (list 
    +negation-char+))

(defvar +binary-operators-char+
  (list 
    +imply-char+
    +equivalence-char+
    +conjunctive-char+
    +disjunctive-char+))

(defvar +quantifiers-char+ 
  (list 
    +forall-char+
    +exists-char+))

