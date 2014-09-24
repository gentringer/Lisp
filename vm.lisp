(load "li2vm")

;la pile d'excution
(defparameter *stack* (make-array 0 :fill-pointer 0
									:adjustable t))

;la pile des variables
(defparameter *vars* (make-array 0 :fill-pointer 0
									:adjustable t))

(defparameter *env* (make-array 0 :fill-pointer 0
									:adjustable t))

; le byte code des fonctions
; make-hash-table = dictionnaire de key-values
(defparameter *bytecode* (make-hash-table))

;Fonction demander
(defun VAR-N  (n)
	; vector-push-extend : extend ajouter des nouvelles valeurs sur la pile
	; elt : récupérer un élément dans l'environnement à une position
	(vector-push-extend (elt *env* (- (length *env*) n)) *vars* )
)
(defun CONST-N  (n)
	(vector-push-extend n *vars* )
)
(defun SKIPNIL  (n)
	; test si la veleur obtenue est nil
	(if (eql (vector-pop *vars*) nil) (SKIP n)) 
)

(defun SKIP  (n)
	; dépile n fois
	(loop for i from  (- n 1) downto 0  do 
		(vector-pop *stack*)
	)
)

(defun RTN ()
	(vector-pop *env*)
)


;excute d'une fonctions
(defun LOAD-N (namefonction)
	(if (gethash namefonction *bytecode*) 
		;S'il trouve namefonction dans le bytecode il fait le loop et empile dans la stack (à l'envers)
		(loop for i from  (- (length (gethash namefonction *bytecode*)) 1) downto 0  do 
			(vector-push-extend (elt (gethash namefonction *bytecode*) i) *stack*)
		)
		(print "FUNCTION UNDEFINED")
	)
)


;le chargeur (load-vm) qui fait appel au deux foncions li2vm lisp2li
(defun compile-expr (expr) 
	(let ((compiled (li2vm (lisp2li expr () ) ()))) 
	; met copiled dans gethash
	(setf (gethash (car  compiled) *bytecode*)  (cdr compiled))
	)  
)
;Fonction d'excution
(defun vm-excute (expr)
	(progn 
		; charger le bytecode dans le stack
		(LOAD-N (car expr))
		(print *stack*)
		(if (eq (cadr (vector-pop *stack*)) (length (cdr expr))) 
			; stacker dans l'environnement
			(vector-push-extend (cadr expr) *env*)
			(print "NUMBER PARAMETRE NOT MATCH")
		)
		(vm-excute-byte)
	)
)

;CALL IMPLEMENTATION
(defun CALL-N (expr)
	(cond
		((eql '+ 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (+  x2 x1) *vars*)))
		((eql '- 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (-  x2 x1) *vars*)))
		((eql '< 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (<  x2 x1) *vars*)))
		((eql '> 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (>  x2 x1) *vars*)))
		((eql '<= 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (<=  x2 x1) *vars*)))
		((eql '>= 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (>=  x2 x1) *vars*)))
		((eql '= 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (=  x2 x1) *vars*)))
		((eql '/ 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (/  x2 x1) *vars*)))
		((eql '* 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (*  x2 x1) *vars*)))
		((eql 'eql 	expr)(let ((x1 (vector-pop *vars*))(x2 (vector-pop *vars*)))(vector-push-extend (eql  x2 x1) *vars*)))
				
		((LOAD-N expr))
	)
)


(defun STAK-N (n) 
	(progn
		(loop for i from  n downto 1  do 
			(vector-push-extend (vector-pop *vars*) *env*)
		)
	
		
	)
)
; exécute instruction après instruction
(defun vm-excute-byte ()
  (let ((expr (vector-pop *stack*))) 
  (cond
		((eql :CALL 	(car expr)) (progn (CALL-N (cadr expr)) (debug-n 'call) (vm-excute-byte)))
		((eql :STACK 	(car expr)) (progn (STAK-N (cadr expr)) (debug-n 'stack) (vm-excute-byte)))
		((eql :VAR 	(car expr)) (progn (VAR-N (cadr expr))(debug-n 'var)(vm-excute-byte)))
		((eql :CONST 	(car expr)) (progn (CONST-N (cadr expr) )(debug-n 'const) (vm-excute-byte)))
		((eql :SKIPNIL 	(car expr)) (progn (SKIPNIL (cadr expr))(debug-n 'skipnill)  (vm-excute-byte)))
		((eql :SKIP 	(car expr)) (progn (SKIP (cadr expr))(debug-n 'skip)  (vm-excute-byte)))
		; vérifie si la stack est vide, si oui il retourne la valeur, sinon il continue à dépiler 
		((eql :RTN 	(car expr)) (progn (rtn) (debug-n 'rtn) (if (eql (length *stack*) 0) (vector-pop *vars*)    (vm-excute-byte)
											)))
	)
	))

(defun debug-n (where)
(progn
	;(print where)
	;(print *vars*)
	;(print *env*)
	;(print *stack*)
	;(read)
	)
)

;(vm-excute-byte)

;(setf (gethash 'one-entry *my-hash*) "one")
;(gethash 'one-entry *my-hash*)


;for test
;(compile-expr '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))))
;(compile-expr '(defun fact (x) (if (< x 1) 1 (* x (fact (- x 1))))))
;(vm-excute '(fibo 8))
;(vm-excute '(fact 10))
