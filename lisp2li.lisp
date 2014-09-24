; Fonction construisant un environnement local a une fonction
(defun make-stat-env (args pos)
 (if (null args)
     ()
   (if (atom args)
     ; Liste simple
     (cons args pos)
   ; Construction de la liste
   (cons (make-stat-env (car args) pos) (make-stat-env (cdr args) (+ 1 pos))))))


(defun map-lisp2li (expr env)
  (if (atom expr)
      expr
    (cons (lisp2li (car expr) env) (map-lisp2li (cdr expr) env))))

(defun lisp2li (expr env)
; Si c'est un atome
(if (atom expr)
    ; Si c'est un literal
    (if (constantp expr)
        (cons :const expr)
      ; Si c'est une variable connue
	(let ((cell (assoc expr env)))
         (if cell
	     (cons :var (cdr cell))
	     (warn "~s n'est pas une variable" expr))))

 ; Sinon c'est une liste
        ; Si le car n'est pas un symbole
  (let ((fun (car expr)) (args (cdr expr)))
    ;si c'est un quote
    (cond ((eq 'quote fun)
	   (cons :const (first args)))
	  ;cas du if
	  ((eq 'if fun)
	   (list :if
		 (lisp2li (first args) env)
		 (lisp2li (second args) env)
		 (lisp2li (third args) env)))
	    ;cas du progn
	  ((eq 'progn fun)
	   (cons :progn (map-lisp2li (cdr expr) env)))
	   ;cas du cond
	  ((eq 'cond fun)
	   (lisp2li (macroexpand-1 expr) env))
	   ;cas du case
	  ((eq 'case fun)
	   (lisp2li (macroexpand-1 expr) env))
	   ;cas du let
	  ((eq 'let fun)
	   (list :let (+ 1 (length (cdr expr))) (map-lisp2li (cadr expr) env)))
           	
        ; cas du defun
	  ((eql 'defun fun)
	   (lisp2li (list 'set-defun (list 'quote (second expr))  
			  (list 'quote (cons (+ 1 (length (third expr)))
					     (if (cddddr expr)
						 (cons :progn (map-lisp2li (cdddr expr) (make-stat-env (third expr) 1)))
					       (lisp2li (cadddr expr) (make-stat-env (third expr) 1 ))))))
		    ()))
	   
	  ;cas d'une fonction méta-définie
	  ((get-defun fun)
	   (cons :mcall (cons fun (map-lisp2li args env))))
	   ;cas d'une fonction inconnue
	  ((not (fboundp fun))
	   (cons :unknown (cons expr env)))
	   ;Fonction pré-définie
	  (t(cons :call (cons (car expr) (map-lisp2li (cdr expr) env))))))))
	   


(defun get-defun (symb)
  (get symb :defun))

(defun set-defun(symb expr)
  (setf (get symb :defun) expr))


;(make-stat-env '(x y z) 1)
;(lisp2li '(if (> 3 4) (if (eq 5 5) (+ 4 3) (- 5 8))) ())
;(lisp2li '(defun fact (x) (if (< x 1) 1 (* x (fact (- x 1))))) ())

