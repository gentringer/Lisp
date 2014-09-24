;;;;;;;;;;; Meta-evaluateur ;;;;;;;;;;;;;;
(load "lisp2li.lisp")

;Meta-evalue une expression
(defun meval (expr)
  (eval-li (lisp2li expr ()) ()))

; Evalue des expressions du langage intermediaire
(defun eval-li (expr env)
  (case (car expr)
	(:const (cdr expr))
	(:var (aref env (cdr expr)))
	(:set-var (setf (aref env (cadr expr))
			(eval-li (cddr expr))))
	(:if (if (eval-li (second expr) env)
		 (eval-li (third expr) env)
	       (eval-li (fourth expr) env)))
	(:progn (map-eval-li (cdr expr) env))
	(:call (apply (second expr) (map-eval-li (cddr expr) env)))
	(:mcall (let ((fun (get-defun (second expr))))
		  (eval-li (cdr fun) (make-env-eval-li (car fun) (cddr expr) env))))
        
	(:unknown (eval-li (lisp2li (second expr) (cddr expr)) env))
	)
  )

;Decoupe les listes a evaluer
(defun map-eval-li (lexpr env)
  (if (null lexpr)
      ()
    (cons (eval-li (car lexpr) env) (map-eval-li (cdr lexpr) env))))

;Construit l'environnement de l'evaluateur
(defun make-env-eval-li (taille args oenv)
   (map-eval-li-array  args oenv 1 (make-array taille)))


(defun map-eval-li-array (args oenv pos nenv)
  (if (null args)
     nenv
   (progn (setf (aref nenv pos) (eval-li (car args) oenv))
	   (map-eval-li-array (cdr args) oenv (+ 1 pos) nenv))))


;(meval '(defun fact (x) (if (< x 1) 1 (* x (fact (- x 1))))))

;(lisp2li '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))) ())

;(eval-li '(:CALL SET-DEFUN (:CONST . FIBO)  (:CONST 2 :IF (:CALL <= (:VAR . 1) (:CONST . 1)) (:VAR . 1)   (:CALL + (:UNKNOWN (FIBO (- X 1)) (X . 1)) (:UNKNOWN (FIBO (- X 2)) (X . 1)))))())

;(meval '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))))