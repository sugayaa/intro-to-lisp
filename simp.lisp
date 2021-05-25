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

;; write a text line
(defun debug_message (x)
  (write-line x)
  )

;; write a text line, appending a symbol at the end
(defun debug_message_symbol (error_msg symbol_)
  (format t (concatenate 'string "~% " error_msg " ~d") symbol_)
  )

;; write an already made text, just passing a symbol
(defun result_ (x)
  (debug_message_symbol t "~% [*] -- Result of simp: ~d" x)
  )

(defun simp_mult (x)
  ;; multi = OP_A * OP_B
  (cond
    ;; if OP_A is a list, simp(OP_A), but we can't forget the 
    ;; rest of the list, so append: simp('* OP_B) 
    ;; ( simp(OP_A) simp(('* OP_B)) )
    ;; and we need to try to simplify the result as well, so,
    ;; result = ( simp(OP_A) simp(('* OP_B)) )
    ;; return simp( result )
    ((listp (car x))                       (simp (append (simp (car x)) (simp (cdr x)))))

    ;; if OP_B[caddr] is a list, simp('* OP_B)[cdr], but we can't forget the 
    ;; rest of the list, so append to: simp(OP_A) 
    ;; ( simp(OP_A) simp(('* OP_B)) )
    ((listp (caddr x))                     (simp (append (simp (car x)) (simp (cdr x)))))

    ;; if OP_A is 0, or OP_B is 0. Return 0 as result.
    ((or (eq (car x) 0) (eq (caddr x) 0))  (simp (list 0)))

    ;; if OP_A is 1, return simp(OP_B)
    ((eq (car x) 1)                        (simp (cddr x)))

    ;; if OP_B is 1, return simp(OP_A)
    ((eq (caddr x) 1)                      (simp (car x)))

    ;; if we can't simplify the multiplication,
    ;; just return the argument
    (t x)
    )
  )

(defun simp_soma (x)
  ;; soma = OP_A + OP_B
  (cond
    ;; if OP_A is a list, simp(OP_A), but we can't forget the 
    ;; rest of the list, so append: simp('+ OP_B) 
    ;; ( simp(OP_A) simp(('+ OP_B)) )
    ;; and we need to try to simplify the result as well, so,
    ;; result = ( simp(OP_A) simp(('* OP_B)) )
    ;; return simp( result )
    ((listp (car x))                                 (simp (append (simp (car x)) (simp (cdr x)))))


    ;; if OP_B[caddr] is a list, simp('+ OP_B)[cdr], but we can't forget the 
    ;; rest of the list, so append to: simp(OP_A) 
    ;; ( simp(OP_A) simp(('+ OP_B)) )
    ((listp (caddr x)) (simp (append (simp (car x))  (simp (cdr x)))))

    ;; if OP_A is 0, return simp(OP_B)
    ((eq (car x) 0)                                  (simp (nth 2 x)))

    ;; if OP_B is 0, return simp(OP_A)
    ((eq (caddr x) 0)                                (simp (car x)))

    ;; if we can't simplify the sum,
    ;; just return the argument
    (t x)
    )
  )

(defun simp (x)
  ;; (simp OP)
  (cond
    ;; Base case, if simp receives an atom,
    ;; return the atom
    ((atom x)  (list x)) ;;base case

    ;; if OP is a list of length 1,
    ;; a single element list,
    ;; return the element.
    ((and (listp x)  (eq (length x) 1))  x)

    ;; if operator of OP[cadr] contains a sum,
    ;; simp_soma(OP)
    ((eq (cadr x) '+)  (simp_soma x))

    ;; if operator of OP[cadr] contains a multiplication,
    ;; simp_mult(OP)
    ((eq (cadr x) '*)  (simp_mult x))

    ;; if first element of OP is an operator,
    ;; we are receiving a TAIL, e.g.:
    ;;
    ;; ('+ 3) of (2 '+ 3), for instance
    ;;
    ;; we will call it: (OP OP_X)
    ((or (eq (car x) '+)  (eq (car x) '*)) (cond
                                            ;; if simp(OP_X) is an atom, and simp(OP_X) is 0, and OP is sum,
                                            ;; return an empty list
                                            ;; e.g.: (+ (0 * 3)) -> ()
                                            ((and (atom (simp (cadr x))) (eq (car x) '+) (eq (simp (cadr x)) 0)) '())

                                            ;; if simp(OP_X) is an atom, and simp(OP_X) is 1, and OP is mult,
                                            ;; return an empty list
                                            ;; e.g.: (* (0 + 1)) -> ()
                                            ;; because, (3 * (0 + 1)) -> (3)
                                            ((and (atom (simp (cadr x))) (eq (car x) '*) (eq (simp (cadr x)) 1)) '())

                                            ;; if simp(OP_X) is not a list,
                                            ;; return (OP simp(OP_X))
                                            ((not (atom (cadr x))) (append (list (car x)) (simp(cadr x))))

                                            ;; if couldn't simplify,
                                            ;; just return the argument
                                            (t (list x))
                                            ))

    ;; if we couldn't simplify,
    ;; just return the argument
    (t  (list x))
    )
  )



;; ------------------ ;;
;;     TEST-CASES     ;;
;; ------------------ ;;


(debug_message_symbol "(der '(a * (x + b)) 'x) => " (der '(a * (x + b)) 'x) )
(debug_message_symbol "simp(der '(a * (x + b)) 'x) => " (simp (der '(a * (x + b)) 'x)))

(debug_message "")

(debug_message_symbol  "'((3 + 0) + (3))) => " (simp '((3 + 0) + (3))))
(debug_message_symbol "'((3 + 0) + (0 + 0))) => " (simp '((3 + 0) + (0 + 0))))

(debug_message "")

(debug_message_symbol "'(0 + ( b + 0)) => " (simp '(0 + ( b + 0))))
(debug_message_symbol "'((b + 0) + (b + 0)) => " (simp '((b + 0) + (b + 0))))

(debug_message "")

(debug_message_symbol  "'((3 * 1) + (3))) => " (simp '((3 * 1) + (3))))
(debug_message_symbol "'((0 * 0) + (0 + 3))) => " (simp '((0 * 0) + (0 + 3))))
