(defun fl-ev (E)
    (cond 
        ((equal (car E) 'first)
            (caadr E))
        ((equal (car E) 'rest)
            (cdadr E))
        ((equal (car E) 'eq)
            (eq (cadr E) (caddr E)))
        ((equal (car E) '+)
            (+ (cadr E) (caddr E)))
        (T E) ; like 'default' in Java switch
    )
)



(defun fl-interp (E P)
    (cond
        ((atom E)
            E)
        ((not (atom (cadr E)))
            (fl-ev
                (list
                    (car E)
                    (fl-interp (cadr E) P)
                )
            )
        )
        ((atom (cadr E))
            (fl-ev E))
        (T E)
    )
)


;; (trace evaluate)
(trace fl-interp)
(trace fl-ev)

;; (fl-interp '(first (1 2)) nil)
;; (fl-interp '(rest (1 2)) nil)
(fl-interp '(rest (1 2 (3))) nil)
(fl-interp '(rest (p 1 2 (3))) nil)
(fl-interp '(first (rest (1 (2 3)))) nil)
(fl-interp '(eq 2 2) nil)
(fl-interp '(eq 2 1) nil)
(fl-interp '(+ 4 1) nil)
;; (fl-interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil)

;; (if x y z)
;; (null x)
;; (atom x)
;; (eq x y) DONE
;; (first x) DONE
;; (rest x) DONE
;; (cons x y)
;; (equal x y)
;; (isnumber x)  return T (true) if x is a number, NIL otherwise. 
;;        Same as (numberp x) in Lisp
;; (+ x y) DONE
;; (- x y)
;; (* x y)
;; (> x y) 
;; (< x y) 
;; (= x y) 
;; (and x y)
;; (or x y)
;; (not x)