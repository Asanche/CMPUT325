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
        ((equal (car E) '-)
            (- (cadr E) (caddr E)))
        ((equal (car E) '*)
            (* (cadr E) (caddr E)))
        ((equal (car E) '>)
            (> (cadr E) (caddr E)))
        ((equal (car E) '<)
            (< (cadr E) (caddr E)))
        ((equal (car E) '=)
            (= (cadr E) (caddr E)))
        ((equal (car E) 'and)
            (and (cadr E) (caddr E)))
        ((equal (car E) 'or)
            (or (cadr E) (caddr E)))
        ((equal (car E) 'equal)
            (equal (cadr E) (caddr E)))
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
;; (fl-interp '(eq 2 2) nil)
;; (fl-interp '(eq 2 1) nil)
;; (fl-interp '(+ 4 1) nil)
(fl-interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil)
(fl-interp '(if (> 1 0) (+ 1 2) (+ 2 3)) nil)
(fl-interp '(if (> 1 0) (if (eq 1 2) 3 4) 5)  nil)
(fl-interp '(and (or T  nil) (> 3 4)) nil)
(fl-interp '(eq (1 2 3) (1 2 3)) nil)
(fl-interp '(eq (1 2 3) (1 2 3)) nil)
(fl-interp '(equal (1 2 3) (1 2 3)) nil)

;; (if x y z)
;; (null x)
;; (atom x)
;; (eq x y)
;; (first x) DONE
;; (rest x) DONE
;; (cons x y)
;; (equal x y) 
;; (isnumber x)  return T (true) if x is a number, NIL otherwise. 
;;        Same as (numberp x) in Lisp
;; (+ x y) DONE
;; (- x y) DONE
;; (* x y) DONE
;; (> x y) DONE
;; (< x y) DONE
;; (= x y) DONE
;; (and x y) DONE
;; (or x y) DONE
;; (not x)