;; Vanderlei de Brito Júnior - XXXXXX
;; Vitor Sugaya              - 743605

(defun der (y x)
  (cond
    ((atom y) (cond
		((equal y x)     1)
		(T 0)
		))
    ((equal (cadr y) '+) 
        (list (der (car y) x) '+ (der(caddr y) x)))
    ((equal (cadr y) '*) 
        (list (list (car y) '* (der (caddr y) x)) '+ (list (der (car y) x)) '* (caddr y)))
    )
  )

(defun debug_message (x)
  (write-line x)
  )

(defun debug_message_symbol (error_msg symbol_)
  (format t (concatenate 'string "~% " error_msg " ~d") symbol_)
  )

(defun result_ (x)
  (debug_message_symbol t "~% [*] -- Result of simp: ~d" x)
  )

(defun simp_mult (x)
  ;; (debug_message_symbol "We are at simp multi, with:" x)
  ;; (format t "~% Hello! from simp_mult, ~d" x)
  (cond
    ((listp (car x))    (append (simp (car x)) (simp (cdr x))))
    ((listp (caddr x))  (append (simp (car x)) (simp (cdr x))))
    ((or (eq (car x) 0) (eq (caddr x) 0)) (simp (list 0)))
    ((eq (car x) 0)     (simp (cddr x)))
    ((eq (caddr x) 1)   (simp (car x)))
    (t x)
    )
  )

(defun simp_soma (x)
  ;; (debug_message_symbol  "We are at simp soma, with:" x)
  ;; (format t "~% Hello! from simp_soma, ~d" x)
  (cond
    ((listp (car x))   (simp (append (simp (car x)) (simp (cdr x))))) ;; we cant lose the rest of the list
    ((listp (caddr x)) (simp (append (simp (car x)) (simp (cdr x))))) ;; we cant lose the rest of the list
    ;; ((listp (caddr x)) (simp (append (simp (car x)) (cons (simp (cadr x)) (simp (caddr x)))))) ;; we cant lose the rest of the list
    ((eq (car x) 0) (simp (nth 2 x)))
    ((eq (caddr x) 0) (simp (car x)))
    (t x) ;;couldnt simp 3:
    )
  )

(defun simp (x)
  ;; (debug_message_symbol "We are at simp, with:" x)
  (cond
    ((atom x) (list x)) ;;base case
    ((and (listp x) (eq (length x) 1))  x)
    ((eq (cadr x) '+) (simp_soma x)) ;;if sum
    ((eq (cadr x) '*) (simp_mult x)) ;;if mult
    ((or (eq (car x) '+) (eq (car x) '*)) (cond
		       ((and (atom (simp (cadr x))) (eq (car x) '+) (eq (simp (cadr x)) 0)) '())
		       ((and (atom (simp (cadr x))) (eq (car x) '*) (eq (simp (cadr x)) 1)) '())
		       ((not (atom (cadr x))) (append (list (car x)) (simp(cadr x))))
		       (t (list x))
		       ))
    (t (debug_message_symbol  "Leaving..." (list x)) (list x)) ;;couldnt simp 3: need more e-girl
    )
  )

;; ------------------ ;;
;;     TEST-CASES     ;;
;; ------------------ ;;


;; (result_ (simp '(0 + 3)))
;; (result_ (simp '(3 + 0)))
;; 
;; (result_ (simp '((3 + 0) + 3)))
;; (result_ (simp '(3 + (3 + 0))))

(debug_message_symbol "[~~] -- Result of der:" (der '(a * (x + b)) 'x) )
(debug_message_symbol "[~~] -- Result of simped der:" (simp (der '(a * (x + b)) 'x)))

(debug_message_symbol  "[~~] -- Result of '((3 + 0) + (3))):" (simp '((3 + 0) + (3))))
(debug_message_symbol "[~~] -- Result of '((3 + 0) + (0 + 0))):" (simp '((3 + 0) + (0 + 0))))

(debug_message_symbol "[~~] -- Result of '(0 + ( b + 0)):" (simp '(0 + ( b + 0))))
