(load "lisp2li.lisp")

; (meval'(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2))))))
; (lisp2li '(defun fibo (n) (if (< n 2) n (+ (fibo (- n 1)) (fibo (- n 2)))))())
; (li2vm '(:CALL SET-DEFUN (:CONST . FIBO) (:CONST 2 :IF (:CALL < (:VAR . 1) (:CONST . 2)) (:VAR . 1) (:CALL + (:UNKNOWN (FIBO (- N 1)) (N . 1)) (:UNKNOWN (FIBO (- N 2)) (N . 1))))) ())



(defun lisp2vm (expr)
  (li2vm (lisp2li expr ()) ()))

(defun li2vm (expr env)
  (if(atom (car expr))
	(cond
		((eql :const (car expr))(list(list ':const (cdr expr))))
		((eql :var (car expr))(list(list ':var (cdr expr))))
		((eql :call (car expr)) (call2vm expr) )
		((eql 'set-defun (car expr)) (defun2vm (cdr expr) ) )
		((eql :unknown (car expr)) (unknown2vm (cdr expr)) )
		((eql :if (car expr))
			( if2vm (car (cdr expr)) (cddr expr))
			)
			
		)
	)
)


;TRAITEMENT DE UNKNOWN
(defun unknown2vm (expr)
	(append  
		(li2vm (lisp2li (cadar expr) (cdr expr))
		       ())
		(list (list :call (car(car expr))) )
	)
)

;TRAITEMENT DE DEFUN
(defun defun2vm (expr)
	(append 
		  (cons (cdr(car expr)) ())
		  (list (list ':STACK (- (cadr(car (cdr expr))) 1)))
		  (li2vm (cddr(car (cdr expr))) ())
	)
)

;converting call
(defun call2vm  (expr) 
	(if (eq 'set-defun (cadr expr)) 
		(li2vm (cdr expr) ())
		;else							
		(append  
			(li2vm (caddr expr) ())
			(li2vm (cadddr expr) ())
			(list(list (car  expr) (cadr  expr)))
		)
	)
)


;TRATEMENT DE IF
;(defun if2vm (args rest)
;	(append 
;		(li2vm args ()) 
;		(list (list ':SKIPNIL (+ 1 (length  (list (car rest))))))
;		(li2vm (car rest) ()) ; l4excution de TRUE
;		(list  (list ':RTN))
;		(li2vm (cadr rest) ()) ; l4excution de FALSE
;		(list  (list ':RTN))
;	)
;)

(defun if2vm (args rest)
	(let ((falsepart  (li2vm (cadr rest) ()) )
		  (truepart  (append (li2vm (car  rest) ()) )))
		(append 
			(li2vm args ()) 
			(list (list ':SKIPNIL (+ 1 (length  truepart))))
			truepart	; l4excution de TRUE
			(list (list ':SKIP (length  falsepart)))
			falsepart ; l'excution de FALSE
			(list  (list ':RTN))
		))
	)

;(lisp2li '(defun fact (x) (if (< x 1) 1 (* x (fact (- x 1))))) ())



	
;(li2vm '(:IF (:CALL > (:CONST . 3) (:CONST . 4)) (:CONST . 1) (:CONST . 5)) ())
;(li2vm '(:+ (:CONST . 3) (:CONST . 4)) ())


;(li2vm '(:IF (:CALL > (:CONST . 3) (:CONST . 4))(:IF (:CALL EQ (:CONST . 5) (:CONST . 5)) (:CONST . 1)(:CONST . 2))(:CONST . 3)) ())			


;(li2vm '(:CALL > (:CONST . 3) (:CONST . 4)) () )

;(li2vm '(:CALL SET-DEFUN (:CONST . F) (:CONST 2 :CALL + (:VAR . 1) (:CONST . 1))) () )


;//PROBLEM
;(li2vm '(:CALL SET-DEFUN (:CONST . FIBO) (:CONST 2 :IF (:CALL < (:VAR . 1) (:CONST . 2)) (:VAR . 1) (:CALL + (:UNKNOWN (FIBO (- X 1)) (X . 1)) (:UNKNOWN (FIBO (- X 2)) (X . 1))))) () )
;(li2vm '(:CALL SET-DEFUN (:CONST . FIBO) (:CONST 2 :IF (:CALL < (:VAR . 1) (:CONST . 2)) (:VAR . 1) (:CALL + (:UNKNOWN (FIBO (- X 1)) (X . 1)) (:UNKNOWN (FIBO (- X 2)) (X . 1))))) () )
;//SOLUTION TEST
;(li2vm '(:UNKNOWN (FIBO (:CALL - (:VAR . 1) (:CONST . 1)))) ())

;//EXMPLE POLYCOPE OKKKKKKKKK DANS CE TEST
;(li2vm '(:CALL SET-DEFUN (:CONST . FIBO) (:CONST 2 :IF (:CALL < (:VAR . 1) (:CONST . 2)) (:VAR . 1) (:CALL + (:UNKNOWN (FIBO (:CALL - (:VAR . 1) (:CONST . 1))) (X . 1)) (:UNKNOWN (FIBO (:CALL - (:VAR . 1) (:CONST . 2))) (X . 1))))) () )



;(lisp2vm '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))))

;(lisp2li '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))) ())

;(li2vm '(:CALL SET-DEFUN (:CONST . FIBO)  (:CONST 2 :IF (:CALL <= (:VAR . 1) (:CONST . 1)) (:VAR . 1)   (:CALL + (:UNKNOWN (FIBO (- X 1)) (X . 1)) (:UNKNOWN (FIBO (- X 2)) (X . 1))))) ())










