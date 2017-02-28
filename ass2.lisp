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
        ((equal (car E) 'cons)
            (cons (cadr E) (caddr E)))
        ((equal (car E) 'if)
            (if (cadr E) (caddr E) (cadddr E)))
        (T E) ; like 'default' in Java switch
    )
)

(defun fl-interp (E P)
    (cond
        ((atom E)
            E)
        ((not(atom (car E))) 
            E)
        ((not (atom (cadr E))); checks first nested value
            (if (null (cddr E))
                (fl-ev ; Handles nested single inputs
                    (list
                        (car E)
                        (fl-interp (cadr E) P)
                    )
                )
                (if (null (cdddr E))
                    (fl-ev ; Handles double inputs like '(equal (1 2 3) (1 2 3))
                        (list
                            (car E)
                            (fl-interp (cadr E) P)
                            (fl-interp (caddr E) P)
                        )
                    )
                    (fl-ev ; Handles double inputs like '(if (> 2 3) (< 2 3) (= 1 2))
                        (list
                            (car E)
                            (fl-interp (cadr E) P)
                            (fl-interp (caddr E) P)
                            (fl-interp (cadddr E) P)
                        )
                    )
                )
            )
        )
        ((atom (cadr E))
            (fl-ev E))
        (T E)
    )
)

;; (trace fl-interp)
;; (trace fl-ev)

(print (fl-interp '(rest (1 2 (3))) nil)) ;; ==> (2 (3))
(print (fl-interp '(rest (p 1 2 (3))) nil)) ;; ==> (1 2 (3))
(print (fl-interp '(first (rest (1 (2 3)))) nil)) ;; ==> (2 3)
(print (fl-interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil)) ;; ==> nil
(print (fl-interp '(if (> 1 0) (+ 1 2) (+ 2 3)) nil)) ;; ==> 3
(print (fl-interp '(if (> 1 0) (if (eq 1 2) 3 4) 5)  nil)) ;; ==> 4
(print (fl-interp '(cons (first (1 2 3))  (cons a nil)) nil)) ;; ==> (1 a)
(print (fl-interp '(and (or T nil) (> 3 4)) nil)) ;; ==> NIL
(print (fl-interp '(eq (1 2 3) (1 2 3)) nil)) ;; ==> NIL
(print (fl-interp '(equal (1 2 3) (1 2 3)) nil)) ;; ==> T

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