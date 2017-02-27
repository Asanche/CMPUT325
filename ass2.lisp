(defun fl-ev (E)
    (cond 
        ((equal (car E) 'first)
            (caadr E))
        ((equal (car E) 'rest)
            (cdadr E))
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
        (T E)
    )
)


;; (trace evaluate)
(trace fl-interp)
(trace fl-ev)

(fl-interp '(first (1 2)) nil)
(fl-interp '(rest (1 2)) nil)
(fl-interp '(rest (1 2 (3))) nil)
(fl-interp '(rest (p 1 2 (3))) nil)
(fl-interp '(first (rest (1 (2 3)))) nil)