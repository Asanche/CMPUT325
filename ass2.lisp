;;;;; HEADER
;; Author: ADAM SANCHE
;; Date: March 1, 2017
;; Class: CMPUT 325 Lec B1
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
                E ;; return E, as it was not a substitutable program application, but needs to be kept
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

(defun fl-interp (E P)
    (handle-e (handle-ps (handle-e E) P))
)

;;;; USEFUL TRACE FUNCTIONS
;; (trace handle-p)
;; (trace handle-ps)
;; (trace handle-e)
;; (trace fl-ev)
;; (trace sub-p)
;; (trace fl-interp)
;; (trace sub-args)
;; (trace sub-arg)
;; (trace get-args)
;; (trace get-func)
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


(fl-interp '(h (g 5)) '((g X = (g (g X))) (h X = b )));; => a  ; for normal order reduction, and 
            ; non-terminating for applicative order reduction