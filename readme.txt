
;;Pour tester lisp2li : 


(load "lisp2li.lisp")

(lisp2li '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))) ())


--------------------------------------------------------------------------------------------------------------------
;;Pour tester eval-li :



(load "eval-li.lisp")

(li2vm '(:CALL SET-DEFUN (:CONST . FIBO)  (:CONST 2 :IF (:CALL <= (:VAR . 1) (:CONST . 1)) (:VAR . 1)   (:CALL + (:UNKNOWN (FIBO (- X 1)) (X . 1)) (:UNKNOWN (FIBO (- X 2)) (X . 1))))) ())

;; ou bien :
 
;;
(meval '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))))


(meval '(fibo 8))



--------------------------------------------------------------------------------------------------------------------

;;Pour tester li2vm et la vm:



(load "vm.lisp")



(lisp2vm '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))))

;; ou bien :

;;
(li2vm '(:CALL SET-DEFUN (:CONST . FIBO)  (:CONST 2 :IF (:CALL <= (:VAR . 1) (:CONST . 1)) (:VAR . 1)   (:CALL + (:UNKNOWN (FIBO (- X 1)) (X . 1)) (:UNKNOWN (FIBO (- X 2)) (X . 1))))) ())


;;chargeur de la machine virtuelle
(compile-expr '(defun fibo (x) (if (<= x 1) x (+ (fibo (- x 1))(fibo (- x 2))))))
;;exécution de la machine virtuelle
(vm-excute '(fibo 8))



--------------------------------------------------------------------------------------------------------------------