(defun rev (l)
    (cond
        ((null l) '())
        (T (append (rev (cdr l)) (list (car l))))))

(defun get-args (A P)
    (if (equal (quote =) (car P))
        A
        (if (null A)
            (get-args (list (car P)) (cdr P))
            (get-args (append (list (car P)) A) (cdr P))
        )
    )
)

(defun get-func (P)
    (if (equal (quote =) (car P))
        (cdr P)
        (get-func (cdr P))
    )
)

(defun sub-p (E P N) ; sub in N then sub in Args
    (cond
        ((null E)
            E)
        ((atom (car E))
            (if (equal (car E) N)
                (append (get-func P) (cdr E))
                E
            )
        )
        (T
            (if (equal (car E) N)
                (append (get-func P) (sub-p (cdr E) P N))
                (append (car E) (sub-p (cdr E) P N))
            )
        )
    )
)

(defun sub-args (E A V)
    (if (null A)
        E
        (if (null E)
            E
            (if (equal (car E) (car A))
                (append (list (car V)) (cdr E))
                (append (list (car E)) (sub-args (cdr E) A V))
            )
        )
    )
)

(defun handle-p (E P A N) ;; A is Args ( X in (f X = (+ 1 X)))
    ;; get first program
    (if (null N)
        (handle-p E P A (caar P))
        (if (null A)
            (handle-p E P (get-args A (cdar P)) N)
            (sub-args (car (sub-p E (car P) N)) A (cdr E))
        )
    )
)

(defun fl-ev (E)
    (cond 
        ((equal (car E) 'atom)
            (atom (cadr E)))
        ((equal (car E) 'null)
            (null (cadr E)))
        ((equal (car E) 'isnumber)
            (numberp (cadr E)))
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



(defun handle-e (E)
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
                        (handle-e (cadr E))
                    )
                )
                (if (null (cdddr E))
                    (fl-ev ; Handles 2 inputs like '(equal (1 2 3) (1 2 3))
                        (list
                            (car E)
                            (handle-e (cadr E))
                            (handle-e (caddr E))
                        )
                    )
                    (fl-ev ; Handles 3 inputs like '(if (> 2 3) (< 2 3) (= 1 2))
                        (list
                            (car E)
                            (handle-e (cadr E))
                            (handle-e (caddr E))
                            (handle-e (cadddr E))
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

(defun fl-interp (E P)
    (handle-e (handle-p (handle-e E) P nil nil))
)

(trace handle-p)
(trace handle-e)
(trace sub-p)
(trace fl-interp)
(trace sub-args)
;; (print (fl-interp '(rest (1 2 (3))) nil)) ;; ==> (2 (3))
;; (print (fl-interp '(rest (p 1 2 (3))) nil)) ;; ==> (1 2 (3))
;; (print (fl-interp '(first (rest (1 (2 3)))) nil)) ;; ==> (2 3)
;; (print (fl-interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil)) ;; ==> nil
;; (print (fl-interp '(if (> 1 0) (+ 1 2) (+ 2 3)) nil)) ;; ==> 3
;; (print (fl-interp '(if (> 1 0) (if (eq 1 2) 3 4) 5)  nil)) ;; ==> 4
;; (print (fl-interp '(cons (first (1 2 3))  (cons a nil)) nil)) ;; ==> (1 a)
;; (print (fl-interp '(and (or T nil) (> 3 4)) nil)) ;; ==> NIL
;; (print (fl-interp '(eq (1 2 3) (1 2 3)) nil)) ;; ==> NIL
;; (print (fl-interp '(equal (1 2 3) (1 2 3)) nil)) ;; ==> T
(fl-interp '(a (+ 1 2)) '((a X = (+ X 1))))
; a function call may be nested

;; (fl-interp '(f (f 2)) '( (f X =  (* X X)) ))
;; (if x y z) DONE
;; (null x) DONE
;; (atom x) DONE
;; (eq x y) DONE
;; (first x) DONE
;; (rest x) DONE
;; (cons x y) DONE
;; (equal x y) DONE
;; (isnumber x) DONE
;; (+ x y) DONE
;; (- x y) DONE
;; (* x y) DONE
;; (> x y) DONE
;; (< x y) DONE
;; (= x y) DONE
;; (and x y) DONE
;; (or x y) DONE
;; (not x) DONE