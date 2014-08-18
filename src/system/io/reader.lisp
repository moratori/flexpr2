

(in-package :cl-user)

(defpackage :flexpr2.system.io.reader
  (:use :cl
        :yacc
        :flexpr2.system.constant.constant
        :flexpr2.system.base.struct)
  (:import-from flexpr2.system.constant.error
      :invalid-token-error
      :invalid-quantifier-error)
  (:export
    :parse
    ))
(in-package :flexpr2.system.io.reader)

#|
  $ 項の定義
  適当な文字は項である: a,b,c,x,y,z
  関数記号をfとして項を有限個とるものは項である: f(a,b,g(x,y)) 


  $ 論理式の定義
  述語記号をPとして項を有限個とるものは論理式である: P(x,f(y,z))
  Xを論理式とするとき ~X は論理式である
  X,Yを論理式とするとき (X V Y) は論理式である
  X,Yを論理式とするとき (X & Y) は論理式である
  X,Yを論理式とするとき (X > Y) は論理式である
  X,Yを論理式とするとき (X - Y) は論理式である
  tを変数、Xを論理式として At.X は論理式である
  tを変数、Xを論理式として Et.X は論理式である
|#


(defun parse (string)
  (parse-with-lexer 
    (lexer string)
    *logical-expression-parser*))

(defun tokenize (string reserved)
  (flet 
    ((trim (x)
       (string-trim '(#\space #\tab #\Newline #\Linefeed) x))
     (cleanup (x)
        (reverse (remove-if 
                  (lambda (each) 
                    (and (typep each 'string)
                         (string= each ""))) x))))
    (loop 
        with result = nil
        with acc    = ""
        finally (return (cleanup (push acc result)))
        for char across (trim string)
        do 
        (if (member char reserved :test #'char=)
          (progn 
            (push (trim acc) result)
            (push char result)
            (setf acc ""))
          (setf acc (concatenate 'string acc (trim (string char))))))))


(defun kind-of (token)
  (cond 
    ((typep token 'string)            :symbol)
    ((char= token +lparen+)           :lparen)
    ((char= token +rparen+)           :rparen)
    ((char= token +delimiter+)        :delimiter)
    ((char= token +separater+)        :separater)
    ((char= token +typing+)           :typing)
    ((char= token +negation-char+)    :negation)
    ((char= token +imply-char+)       :imply)
    ((char= token +equivalence-char+) :equivalence)
    ((char= token +disjunctive-char+) :disjunctive)
    ((char= token +conjunctive-char+) :conjunctive)
    ((char= token +forall-char+)      :forall)
    ((char= token +exists-char+)      :exists)
    (t (error 
         (make-condition 'invalid-token-error :value token)))))


(defun lexer (string)
  (let ((target (tokenize string +reserved+)))
    (lambda ()
      (let ((token (pop target)))
        (if (null token)
          (values nil nil)
          (values (kind-of token) token))))))



(define-parser *logical-expression-parser*
  (:start-symbol expr)

  (:terminals 
    (:symbol 
     :lparen 
     :rparen 
     :delimiter 
     :separater 
     :typing
     :negation 
     :imply 
     :equivalence 
     :disjunctive 
     :conjunctive
     :forall :exists))
  
  (:precedence
    ((:left :delimiter)
     (:left :negation)
     (:left :conjunctive :disjunctive)
     (:left :imply :equivalence) 
     ))

  #|
    precedence の指定は要するに 演算子間の衝突が起こったときに
    使われるものなわけだけど、今のexprの定義が明示的にその衝突を防ぐようになってるのかな
    P & Q > R みたいのが来た時に、P がまず atomic-expr として処理されてしまう
    => っていうかトップレベルで定義しとかないといけないんだ
       expr の下で
  |#

  (expr 

    (:symbol 
      (lambda (symbol)
        (make-literal
          nil
          (intern symbol)
          nil)))

    (:symbol :rparen termseq :lparen
      (lambda (symbol rparen termseq lparen)
        (declare (ignore rparen lparen))
        (make-literal nil (intern symbol) termseq)))

    (:negation expr
      (lambda (op expr)
        (if (typep expr 'literal)
          (progn 
            (setf (negation expr) (not (negation expr)))
            expr)
          (make-connected-logical-expression 
            (make-operator 
              (char-> op)) expr nil))))

    (expr :conjunctive expr
      (lambda (expr1 op expr2)
        (make-connected-logical-expression 
          (make-operator 
            (char-> op)) expr1 expr2)))

    (expr :disjunctive expr
      (lambda (expr1 op expr2)
        (make-connected-logical-expression 
          (make-operator 
            (char-> op)) expr1 expr2)))
    
    (expr :imply expr
      (lambda (expr1 op expr2)
        (make-connected-logical-expression 
          (make-operator 
            (char-> op)) expr1 expr2)))
    
    (expr :equivalence expr
      (lambda (expr1 op expr2)
        (make-connected-logical-expression 
          (make-operator 
            (char-> op)) expr1 expr2)))
    
    (quantseq :delimiter expr
      (lambda (quantseq delimiter expr)
        (declare (ignore delimiter))
        (make-quantifier-logical-expression 
          quantseq expr))) 

    (:rparen expr :lparen 
     (lambda (rp expr lp)
       (declare (ignore rp lp))
       expr)))


  (quant 
    (:forall vterm
      (lambda (quant term)
        (make-quantifier
          (char-> quant)
          term)))

    (:forall :rparen vterm :typing :symbol :lparen
     (lambda (quant rp vterm colon type lp)
       (declare (ignore rp colon lp))
       (make-quantifier
	 (char-> quant)
	 (make-typed-vterm 
	   vterm
	   type))))
    
    (:exists vterm
      (lambda (quant term)
        (make-quantifier
          (char-> quant)
          term)))

    (:exists :rparen vterm :typing :symbol :lparen
     (lambda (quant rp vterm colon type lp)
       (declare (ignore rp colon lp))
       (make-quantifier
	 (char-> quant)
	 (make-typed-vterm 
	   vterm
	   type)))))

  (quantseq 
    (quant #'list)
    (quantseq quant
      (lambda (quantseq quant)
        (append quantseq (list quant)))))

  (term 
    vterm
    fterm)
  
  (vterm 
    (:symbol 
      (lambda (symbol) 
        (make-vterm 
          symbol
          (or 
            (string= (string-upcase symbol) symbol)
            (char= (char symbol 0) +constant-prefix+))))))

  (fterm 
    (:symbol :rparen termseq :lparen 
      (lambda (symbol rparen termseq lparen) 
        (declare (ignore rparen lparen))
        (make-fterm symbol termseq))))
  
  (termseq
    (term   #'list)
    (termseq :separater term 
      (lambda (termseq separater term) 
        (declare (ignore separater))
        (append termseq (list term))))))
 




