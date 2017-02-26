(defun evaluate (E)
    (cond 
        ((equal (car E) 'first)
            (caadr E))
        ((equal (car E) 'rest)
            (cdadr E))
        (T E) ; like 'default' in Java switch
    )
)

(defun fl-interp (E P)
    (if (atom E)
        E
        (if (atom (car E))
            (evaluate (list (car E) (fl-interp (cadr E) P)))
            E ; else
        )
    )
)

(trace evaluate)
(trace fl-interp)

(fl-interp '(rest (1 2 (3))) nil)