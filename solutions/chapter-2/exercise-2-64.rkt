
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.64.
;---
;The following procedure 'list->tree' converts an ordered list to a balanced binary tree.
;The helper procedure 'partial-tree' takes as arguments an integer 'n' and list of at
;least 'n' elements and constructs a balanced tree containing the first 'n' elements of
;the list. The result returned by 'partial-tree' is a pair (formed with cons) whose 'car
;is the constructed tree and whose 'cdr' is the list of elements not included in the
;tree (SEE BELOW *).
;---
;a. Write a short paragraph explaining as clearly as you can how 'partial-tree' works.
;Draw the tree produced by 'list->tree' for the list (1 3 5 7 9 11).
;---
;b. What is the order of growth in the number of steps required by 'list->tree' to
;convert a list of n elements?
;------------------------------------------------------------------------------------------

;'make-tree'
;---
(define (make-tree entry left right)
  (list entry left right))

;(*) 'list->tree'
;---
(define (list->tree-1 elements)
  (car (partial-tree elements (length elements))))

;(*) 'partial-tree' -> helper for 'list->tree'
;(note: 'quotient' is a procedure of two arguments 'a' and 'b' that performs the
;operation (floor (/ a b)), that is, divides 'a' by 'b' and if the result 'r' of the
;division is not an integer, it is rounded to the nearest integer smaller than 'r')
;---
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;ANSWER TO PART A
;---
;'partial-tree' recursively splits the list in three parts: its median value, a list of
;values to the left of the median and a list of values to the right of the median. the
;first serves as the entry at that point, and the second and third as the left as right
;trees, respetively. doing this, ensures that, at every split, the number of elements to
;the left of the entry is either 'r' or 'r-1', with 'r' equal to the number of elements
;to the right of the entry. for example, let L be the list '(1 2 3 4 5 6 7 8 9 10).
;following this idea, by evaluating (list->tree L) we get a process that evolves somewhat
;like (each caret points to the median value of the corresponding list):
;---
(display "PART A (example)") (newline)
;---
(list->tree-1 '(1 2 3 4 5 6 7 8 9 10))
;(1 2 3 4 5 6 7 8 9 10)
;         ^
;(5 (1 2 3 4) (6 7 8 9 10))
;      ^           ^
;(5 (2 (1) (3 4)) (8 (6 7) (9 10)))
;           ^         ^
;(5 (2 (1 () ()) (3 () (4))) (8 (6 () (7)) (9 () (10))))
'(5 (2 (1 () ()) (3 () (4 () ()))) (8 (6 () (7 () ())) (9 () (10 () ()))))

;ANSWER TO PART B
;---
;for an excellent explanation on the order of growth of the procedure (as well as a much
;more elucidative answer to question (a), visit JoT's Jottings' blog post at
;http://jots-jottings.blogspot.com/2011/12/sicp-exercise-264-constructing-balanced.html)

;EXTRA 1 (alternative implementation)
;---
;a more straightforward implementation of 'list->tree' (although certainly less efficient
;than the original) is presented below
;---
(define (list-ref n lst)
  (if (= n 0)
      (car lst)
      (list-ref (- n 1) (cdr lst))))
;---
(define (to-left n lst)
  (if (= n 0)
      '()
      (cons (car lst)
            (to-left (- n 1) (cdr lst)))))
;---
(define (to-right n lst)
  (if (= n 0)
      (cdr lst)
      (to-right (- n 1) (cdr lst))))
;---
(define (list->tree-2 lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (make-tree (car lst) '() '()))
        (else
         (let ((mid (quotient (length (cdr lst)) 2)))
           (make-tree (list-ref mid lst)
                      (list->tree-2 (to-left mid lst))
                      (list->tree-2 (to-right mid lst)))))))

;EXTRA 2 (alternative implementation)
;---
;by collecting the parts of a split with a single procedure call (and thus, only parsing
;the list once), we get a more efficient implementation than the previous
;---
(define (add elem lst)
  (if (null? lst)
      (cons elem '())
      (cons (car lst)
            (add elem (cdr lst)))))
;---
(define (split-list n lst)
  (define (iter-list n lst res)
    (if (= n 0)
        (list (car lst) res (cdr lst))
        (iter-list (- n 1)
                   (cdr lst)
                   (add (car lst) res))))
  (iter-list n lst '()))
;---
(define (list->tree-3 lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (make-tree (car lst) '() '()))
        (else
         (let ((mid (quotient (length (cdr lst)) 2)))
           (let ((parts (split-list mid lst)))
             (make-tree (car parts)
                        (list->tree-3 (cadr parts))
                        (list->tree-3 (caddr parts))))))))

;TESTS
;---
(display "---") (newline)
(display "TEST (all versions of 'list->tree')") (newline)
;---
(list->tree-1 '(1 3 5 7 9 11))
(list->tree-2 '(1 3 5 7 9 11))
(list->tree-3 '(1 3 5 7 9 11))

