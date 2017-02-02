;; Author: Adam Sanche, 1393331
;; Class: CMPUT 325 W2017
;; Section: LEC B1

;QUESTION 1
;First checks if X is NIL, if so it returns NIL. Else it then
;checks if (car X) is equal to Y, if so return T. Else recursion
;with X = (cdr X) occurs
(defun xmember (X Y)
    (if (equal NIL X) NIL
        (if (equal (car X) Y) T 
            (xmember (cdr X) Y)
        )
    )
)

;QUESTION 2
;Checks the type of the first element of the list. If the first element
;is also a list, it extracts the values from the list, constructs a list with them,
;then flattens them. Remove NIL handles cases like (a) where it will return a NIL when 
;flattened. If the first element is not a list, it check the rest of the list to see if 
;it is a list. If it is a new list is constructed and returned from the first element 
;and the rest of the list flattened. NILs are once again removed. If the first and rest
;of the list aren't themselves lists (like (a b c d)) the list is simply returned.
(defun flatten (X)
    (if (typep (car X) 'CONS) 
        (remove NIL (flatten (cons (caar X) (cons (cdar X) (cdr X)))))
        (if (typep (cdr X) 'CONS)
            (cons (car X) (remove NIL (flatten (cdr x))))
            X
        )
    )
)

;QUESTION 3
;This first check whether either list is NIL. If either one is NIL, it returns the other list.
;It then constructs a list from the first element of Y, first element of X, and the mix of 
;the rest of X and the rest of Y
(defun mix (X Y)
    (if (eq NIL X)
        Y
        (if (eq NIL Y)
            X
            (cons (car Y) (cons (car X) (mix (cdr X) (cdr Y))))
        )
    )
)

;QUESTION 4
;first check if L is null. If it is, construct list of (NIL NIL). Else remove NILs from
;a list of two lists (of form ((L1) (L2)). The first nested list is constructed from the first element in L and 
;the first element in (split (cddr L)). (cddr L) give the third element onward in list L, essentially
;removing the first two elements from L. The second nested list is constructed from the second element and
;the rest (minus first) element of (split (cddr L)). Once again (cddr L) returns L minus the first two elements.
;It is safe to use car and cdr, as we control the output of split. The output is always of form ((L1) (L2)) so
;(car (split L)) always returns L1 and (cadr (split L)) always returns L2. We essentially build the alternating element
;lists from L by exploiting our control over the output of split.
(defun split (L)
    (if (eq L NIL)
        (cons L (cons L L))
        (if (> (list-length L) 1)
            (list 
                (cons (car L) (car (split (cddr L))))
                (cons (cadr L) (cadr (split (cddr L))))
            )
            L
        )
    )
)

;QUESTION 5.1
;No. This is exemplified by different length lists. Particularly any lists L1 L2
;such that the difference in lengths in 2 or greater (although a difference of 1
;could result in different L1' L2' output)
;
;E.G. (split (mix '(A B C D) '(E F))) = (split '(E A F B C D)) = ((E F C) (A B D)) =/= ((A B C D) (E F))

;QUESTION 5.2
;Yes, it will always return L.
;
;PROOF: 
;By the nature of split, (split L), where L is (L0 L1 L2 ... Ln) will result in 
;((L0 L2 ... Ln) (L1 L3 ... Ln-1)) where n is even
;or ((L0 L2 ... Ln-1) (L1 L3 ... Ln)) where n is odd. 
;These will always combine back via mix, as the first element of the left list is chosen, then
;the first element of the right list is chosen etc... to be 
;(L0 L1 L2 L3 ... Ln), which is in turn just L.

;QUESTION 6
;A simple recursive summation function. Just adds car to the sun of cdr.
;Assumes no nested lists and only number input
(defun sum (L)
    (if (equal L NIL)
        0 
        (+ (car L) (sum (cdr L)))
    )
)

;A function that removes a element of a list at a particular index.
;This function is not recursive, but instead relies on nthcdr and reversing the list a few times.
;It essentially appends the list after the element to be removed with the list after the element
;to be removed.
(defun remelement (n L)
    (if (= 1 n)
        (cdr L)
        (if (= n (list-length L))
            (reverse (cdr (reverse L)))
            (append (reverse (nthcdr (- (list-length L) (- n 1)) (reverse L))) (nthcdr n L))
        )
    )
)

;This function finds the subset of the list L that sums up to the sum S, or returns NIL.
;First it checks if L is NIL. If so, it returns NIL, else it checks if (sum L) equals S.
;If so it returns the list, else it check if (sum L) is less than S. If this is the case,
;we cannot remove any elements to reach S as removing an element will result in an even smaller
;sum. If it is smaller we return NIL, else we loop through the elements, removing one element at a time 
;(from L0 to Ln), and recursively calling subsetsum on the reduced set, only returning from the loop when
;the result is not NIL meaning that it found an appropriate subset of L to sum up to S, or when all combinations
;have been tried, and all results were NIL.
(defun subsetsum (S L)
    (if (equal L NIL) 
        NIL
        (if (equal (sum L) S)
            L
            (if (< (sum L) S)
                NIL
                (loop for x from 1 to (list-length L)
                    for y = (subsetsum S (remelement x L))
                    when (not (equal y NIL))
                        return y
                )
            )
        )
    )
)