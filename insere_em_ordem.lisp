(defun insere_em_ordem (x y)
  (cond
    ((or (null x) (eq (car x) y) (> (car x) y)) (cons y x))
    (t (cons (car x) (insere_em_ordem (cdr x) y) ))
    )
  )

(format t "~% output: ~d" (insere_em_ordem '(1 2 4) 3))
(format t "~% output: ~d" (insere_em_ordem '(1 2 3 4) 3))
(format t "~% output: ~d" (insere_em_ordem '(1 2) 3))
(format t "~% output: ~d" (insere_em_ordem '() 3))
