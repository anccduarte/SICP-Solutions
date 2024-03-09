
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.17.
;---
;Devise a correct version of the 'count-pairs' procedure of Exercise 3.16 that returns
;the number of distinct pairs in any structure. (Hint: Traverse the structure,
;maintaining an auxiliary data structure that is used to keep track of which pairs have
;already been counted.)
;------------------------------------------------------------------------------------------

;'count-pairs'
;(this new version keeps a list structure of visited pairs and only increments the count
;of pairs if the current pair isn't already present in this list; in this case, the list
;structure is updated to include the new pair)
;---
(define (count-pairs x)
  ;---
  (let ((visited '()))
    ;---
    (define (to-add p)
      (if (memq p visited)
          0
          (begin (set! visited (cons p visited))
                 1)))
    ;---
    (define (count-aux x)
      (if (not (pair? x))
          0
          (+ (count-aux (car x))
             (count-aux (cdr x))
             (to-add x))))
    ;---
    (count-aux x)))

;test with examples from Exercise 3.16
;---
(define r3 '(a b c))
(count-pairs r3)
;---
(define x '(a))
(define y (cons x x))
(define r4 (list y))
(count-pairs r4)
;---
(define r7 (cons y y))
(count-pairs r7)

