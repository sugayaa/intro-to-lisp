(defun primeiro_elemento (x y z pos)
    (cond
      ((null x) nil)
      ((atom x) nil)
      ((equal (car x) y) (segundo_elemento (cdr x) y z pos)) 
      (t (primeiro_elemento (cdr x) y z (+ pos 1)))
    )
)

(defun segundo_elemento (x y z pos)
  (cond
    ((null x) nil)
    ((atom x) nil)
    ((equal (car x) z) pos)

    ;; don't enter sublists by accident
    ((eq (length x) 1) nil)
    (t (primeiro_elemento (car x) y z (+ pos 1)))
  )
  )

;; wrapper
(defun acha_seguidos (x y z)
  (primeiro_elemento x y z 0)
)

(format t "~% output ~d" ( acha_seguidos '(1 2 3) 1 2 ))
(format t "~% output ~d" ( acha_seguidos '(1 2 3) 1 3 ))
(format t "~% output ~d" ( acha_seguidos '(1 (2) 3 4) '(2) 3 ))
(format t "~% output ~d" ( acha_seguidos '(1 (2) 3 (4)) 3 '(4)))
