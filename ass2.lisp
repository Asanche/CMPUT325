;;;;; HEADER
;; Author: ADAM SANCHE
;; Date: March 1, 2017
;; Class: CMPUT 325 Lec B1
;;;; NOTES
;; I tested, and double tested all of the test cases, and unless I read them wrong, this works. :D
;;;;

(defun sub-arg (E A) ;; of form ( (K . V) (K . V) ... )
    (cond
        ((null E) ;; If E is nil, we have reached the end, and are done subbing values into E
            nil ;; return nil to signify finished
        )
        ((atom (car E)) ;; If the first value is an atom, we can replace it safely if necessary
            (if (equal (caar A) (car E)) ;; If the first value of A (K) equals the first value of E, we found an instance of the argument
                (cons (cdar A) (sub-arg (cdr E) A)) ;; Use (cadr A) b/c it gives the value as a list, so it will nest it as it was already nested >.> Recurr as well over cdr E
                (cons (car E) (sub-arg (cdr E) A)) ;; No match, but still recur over rest of E
            )
        )
        ((not (atom (car E))) ;; If the first value of A is not an atom, we need to recur in two directions... on car and cdr to unnest the expression for substitution
            (cons (sub-arg (car E) A) (sub-arg (cdr E) A)) ;; Recombine them after execution ofc
        )
    )
)

(defun sub-args (E A)
    (if (null A) ;; If A is null, we have iterated over all Arguments to be subsituted
        E
        (sub-args (sub-arg E A) (cdr A)) ;; We iteratre through A, using the resultant E as our next E
    )
)

(defun get-func (P) ;; This gets the value from P after the equals sign
    (if (equal (car P) (quote =)) ;; If we reach the '=', all to the right is the program application
        (if (atom (cadr P)) ;; If the function is an atom, it can cause errors later, so return it as a list if this is the case
            (cdr P) ;; return this case
            (cadr P) ;; The rest is the func, but it is nested as ((* a b)) etc...
        )
        (get-func (cdr P)) ;; recur across P until '='
    )
)

(defun get-args (P V)
    (cond
        ((or (null P) (null V)) ;; If these values are nil, we should not return anything confusing
            nil ;; Exit with nil
        )
        ((equal (cdr P) (quote =)) ;; If the first value of P is '=', we have found all of the arguments for that program
            (car V) ;;
        )
        (T
            (cons (cons (car P) (car V)) (get-args (cdr P) (cdr V)))
        ) ;; Contruct K.V Pairs from current K.V Pair and all others
    )
)

(defun handle-p (E P) ;; Get All Argument K.V Pairs, then sub the program into the values in the K.V Pairs
    (cond
        ((null E) ;; If E is nil, we passed in () as (cdr E) b/c E was ((a 2 3)) or something
            nil ;; return nil as cons doesnt care
        )
        ((atom E)
            E ;; return E, as it is not evaluatable
        )
        ((atom (car E)) ;; If the first element of E is an atom, we have unnested a program name (built in like '*', user defined via P, or undefined, and thusly an error)
            (if (equal (car P) (car E)) ;; if the program name matches P, gogogo
                (sub-args (get-func P) (get-args (cdr P) (handle-p (cdr E) P))) ;; Here we will replace the name with the program, and evaluate it using the args that should be specified to the right
                (if (null (cdr E)) ;; Weird stuff can happen where nested recursive function (PUSH) might not actually be nested at the start of a lust (1 PUSH (2 3) 4) >.>
                    E ;; return E, as it was not a substitutable program application, but needs to be kept
                    (cons (car E) (handle-p (cdr E) P)) ;; Handle PUSH example above
                )
            )
        ) ;; We pass (cdr P) to get-args to skip the name of the function
        ((not (atom (car E))) ;; If the first element is't an atom, we have a further nested expression like ((F 1 2) (A 3 4))
            (cons (handle-p (car E) P) (handle-p (cdr E) P)) ;; We will pass the first and rest to handle-p, unnesting the list one layer. Further nesting is handled implicity e.g. ((F (A (1 2)) (A 1 2))
        ) 
    )
)

(defun handle-ps (E P) 
    (if (null P) ;; If P is nil, we are at the end of the list of P's and we return
        E 
        (handle-ps (handle-p E (car P)) (cdr P)) ;; Recurr, passing in a new P to handle-p every time. 
    )
)

(defun fl-ev (E) ;; Here we simly 'switch' on the various operands available to 'FL'
    (cond 
        ((equal (car E) 'atom)
            (atom (cadr E)))
        ((equal (car E) 'null)
            (null (cadr E)))
        ((equal (car E) 'not)
            (not (cadr E)))
        ((equal (car E) 'isnumber)
            (numberp (cadr E)))
        ((equal (car E) 'first)
            (caadr E))
        ((equal (car E) 'rest)
            (cdadr E))
        ((equal (car E) 'eq)
            (eq (cadr E) (caddr E)))
        ((and (and (equal (car E) '+) (numberp (cadr E))) (numberp (caddr E)))
            (+ (cadr E) (caddr E)))
        ((and (and (equal (car E) '-) (numberp (cadr E))) (numberp (caddr E)))
            (- (cadr E) (caddr E)))
        ((and (and (equal (car E) '*) (numberp (cadr E))) (numberp (caddr E)))
            (* (cadr E) (caddr E)))
        ((and (and (equal (car E) '>) (numberp (cadr E))) (numberp (caddr E)))
            (> (cadr E) (caddr E)))
        ((and (and (equal (car E) '<) (numberp (cadr E))) (numberp (caddr E)))
            (< (cadr E) (caddr E)))
        ((and (and (equal (car E) '=) (numberp (cadr E))) (numberp (caddr E)))
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
        
        (T E) ; like 'default' in Java switch - Return E if it doesnt match anything
    )
)

(defun handle-e (E)
    (cond
        ((or (null E) (atom E))  ;; If E is null or an atom, it can't be evaluated
            E ;; return E if this is the case, as it may be part of an evaluation still
        )
        ((atom (car E)) ;; Operator found, evaluate it
            (fl-ev (cons (car E) (handle-e (cdr E)))) ;; evaluate the current operator w.r.t the handled cdr of E. Recurr on the rest of the list
        )
        ((not (atom (car E))) ;; Similar to above, we need to unnest the expression if the first value isnt an atom
            (fl-ev (cons (handle-e (car E)) (handle-e (cdr E)))) ;; Recur, on car and cdr, effectively unnesting E
        )
        (T 
            E ;; Return E by default
        )
    )
)

(defun keep-handling (E P)
    
)


(defun fl-interp (E P)
    (cond
        ((not (equal (handle-e (handle-ps (handle-e E) P)) E))
            (fl-interp (handle-e (handle-ps (handle-e E) P)) P)
        )
        (T 
            E
        )
    )
)

;;;; USEFUL TRACE FUNCTIONS
;; (trace handle-p)
;; (trace handle-ps)
;; (trace handle-e)
;; (trace fl-ev)
;; (trace fl-interp)
;; (trace sub-args)
;; (trace sub-arg)
;; (trace get-args)
;; (trace get-func)