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
                (append (get-func P) (sub-p (cdr E) P N))
                E
            )
        )
        ((not (atom (car E)))
            (if (equal (caar E) N)
                (append (get-func P) (sub-p (cadr E) P N))
                (append (caar E) (sub-p (cadr E) P N))
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

(defun sub-arg (E A V)
    (cond 
        ((null E) 
            E)
        ((atom E)
            (if (equal E (car A))
                (list (car V))
                E
            )
        )
        ((equal (car E) (car A))
            (append (list (car V)) (cdr E)))
        ((atom (cadr E))
            (append (list (car E)) (sub-arg (cdr E) A V)))
        (T 
            (append (list (car E)) (append (list (sub-arg (cadr E) A V)) (sub-arg (cddr E) A V))))
    )
)

(defun sub-args (E A V)
    (if (null A)
        E
        (sub-args (sub-arg E A V) (cdr A) (cdr V))
    )
)

(defun handle-ps (E P) ;; A is Args ( X in (f X = (+ 1 X)))
    ;; get first program
    (if (null P)
        E
        (handle-ps (handle-p E (car P) nil nil) (cdr P))
    )
)

(defun handle-p (E P A N) ;; A is Args ( X in (f X = (+ 1 X)))
    ;; get first program
    (if (null P)
        E
        (if (null N)
            (handle-p E P A (car P))
            (if (null A)
                (handle-p E P (get-args A (cdr P)) N)
                (sub-args (car (sub-p E P N)) A (cdr E))
            )
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
        ((or (null E) (atom E)) E)
        ((atom (car E))
            (fl-ev (cons (car E) (handle-e (cdr E)))))
        ((not (atom (car E)))
            (fl-ev (cons (handle-e (car E)) (handle-e (cdr E)))))
        (T E)
    )
)

(defun fl-interp (E P)
    (handle-e (handle-ps (handle-e E) P))
)

;; (trace handle-p)
;; (trace handle-e)
;; (trace fl-ev)
;; (trace sub-p)
;; (trace fl-interp)
;; (trace sub-args)
;; (trace sub-arg)
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
(print (fl-interp '(a (+ 1 2)) '((a X = (+ X 1))))) ;; ==> 4
(print (fl-interp '(a (+ 1 2) (+ 2 3)) '((a X Y = (+ X Y)))));; ==> 8
(print (fl-interp '(a (+ 1 2) (+ 2 3) (+ 6 6)) '((a X Y Z = (+ X (+ Y Z)))))) ;; ==> 20
; a function call may be nested

;;More Samples
; a program may be empty. 

(print (fl-interp '(+ 1 2) nil)) ;; => 3

; a function call may be nested

(print (fl-interp '(f (f 2)) 
        '( (f X =  (* X X)) )
)) ;; =>  16

; function symbols may be quite arbitrary

(print (fl-interp '(a (+ 1 2)) 
        '( (a X = (+ X 1)) )
)) ;; => 4

(print (fl-interp '(b (+ 1 2)) 
        '( (b X = (+ X 1)) )
)) ;; => 4


(print (fl-interp '(h (g 5))
    '(  (g X = (g (g X)))
        (h X = a )  )
)) ;; => a  ; for normal order reduction, and 
            ; non-terminating for applicative order reduction


; But don't use setq in your program, it's not allowed!

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