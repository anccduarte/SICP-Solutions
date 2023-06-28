
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 2.63.
;---
;Each of the following two procedures converts a binary tree to a list (SEE BELOW *).
;---
;a. Do the two procedures produce the same result for every tree? If not, how do the
;results differ? What lists do the two procedures produce for the trees in Figure 2.16?
;---
;b. Do the two procedures have the same order of growth in the number of steps required
;to convert a balanced tree with 'n' elements to a list? If not, which one grows more
;slowly?
;------------------------------------------------------------------------------------------


;constructor for trees
;---
(define (make-tree entry left right)
  (list entry left right))

;selectors for trees
;---
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

;(*) version 1 of 'tree->list'
;---
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;(*) version 2 of 'tree->list'
;---
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;------------------------------------------------------------------------------------------
;PART A
;------------------------------------------------------------------------------------------

;trees from Figure 2.16. -> representing the set {1,3,5,7,9,11}
;---
(define tree1 '(7
                (3
                 (1 () ())
                 (5 () ()))
                (9
                 ()
                 (11 () ()))))
;---
(define tree2 '(3
                (1 () ())
                (7
                 (5 () ())
                 (9
                  ()
                  (11 () ())))))
;---
(define tree3 '(5
                (3
                 (1 () ())
                 ())
                (9
                 (7 () ())
                 (11 () ()))))

;both procedures produce the same result for an arbitrary tree, since they both perform
;"left-to-right" traversal. the result is, then, an ordered list. as an example, consider
;the evolution of the processes which arise by evaluating 'tree->list-1' and
;'tree->list-2' on 'tree1'
;---
(display "PART A -> evolution of (tree->list-1 tree1)")
(newline)
;---
(tree->list-1 '(7
                (3 (1 () ()) (5 () ()))
                (9 () (11 () ()))))
;---
(append (tree->list-1 '(3 (1 () ()) (5 () ())))
        (cons 7
              (tree->list-1 '(9 () (11 () ())))))
;---
(append (append (tree->list-1 '(1 () ()))
                (cons 3
                      (tree->list-1 '(5 () ()))))
        (cons 7
              (append '()
                      (cons 9
                            (tree->list-1 '(11 () ()))))))
;---
(append (append (append '() (cons 1 '()))
                (cons 3
                      (append '() (cons 5 '()))))
        (cons 7
              (append '()
                      (cons 9
                            (append '() (cons 11 '()))))))
;---
;...
;the majority reduction steps are omitted for the sake of sanity
;---
(append '(1 3 5) (cons 7 '(9 11)))
'(1 3 5 7 9 11)
;---
(display "---") (newline)
(display "PART A -> evolution of (tree->list-2 tree1)")
(newline)
;---
(tree->list-2 '(7
                (3 (1 () ()) (5 () ()))
                (9 () (11 () ()))))
;---
;(copy-to-list '(7
;                (3 (1 () ()) (5 () ()))
;                (9 () (11 () ())))
;              '())
;---
;(copy-to-list '(3 (1 () ()) (5 () ()))
;              (cons 7
;                    (copy-to-list '(9 () (11 () ()))
;                                  '())))
;---
;(copy-to-list '(3 (1 () ()) (5 () ()))
;              (cons 7
;                    (copy-to-list '()
;                                  (cons 9
;                                        (copy-to-list '(11 () ())
;                                                      '())))))
;---
;(copy-to-list '(3 (1 () ()) (5 () ()))
;              (cons 7
;                    (cons 9
;                          (copy-to-list '()
;                                        (cons 11
;                                              (copy-to-list '() '()))))))
;---
;(copy-to-list '(3 (1 () ()) (5 () ()))
;              (cons 7
;                    (cons 9
;                          (cons 11 '()))))
;---
;...
;the same reasoning applies to the left branch of the original tree. ultimately, the
;result will be (cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (cons 11 '())))))), which is
;readily converted to '(1 3 5 7 9 11)
;---
(cons 1 (cons 3 (cons 5 (cons 7 (cons 9 (cons 11 '()))))))
'(1 3 5 7 9 11)


;------------------------------------------------------------------------------------------
;PART B
;------------------------------------------------------------------------------------------

;it is clear that 'tree->list-1' is a procedure which evolves a recursive process, whilst
;its counterpart 'tree->list-2' evolves an iterative process (the state of the process is
;captured at any step by observing the state variables 'tree' and 'result-list'). the
;main structural difference between the two procedures is that whilst the former performs
;a call to 'append' to merge the left and right branches of the tree, the latter employs
;a smarter strategy of calling, in place of 'append', 'copy-to-list', which ultimately
;reduces to a call to 'cons'. in the case of 'tree->list-2', no redundant calls to 'cons'
;are made. however, there are many calls to 'append' that could be avoided in
;'tree->list-1'
;---
;not sure how to compute the orders of growth, but pretty sure that 'tree->list-1' has a
;higher time complexity (because of the calls to 'append')

