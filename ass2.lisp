(defun fl-ev (E)
    (cond 
        ((equal (car E) 'first)
            (caadr E))
        ((equal (car E) 'rest)
            (cdadr E))
        ((equal (car E) 'eq)
            (eq (cadr E) (caddr E)))
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