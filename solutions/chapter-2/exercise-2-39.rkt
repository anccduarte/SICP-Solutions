
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.39.
;---
;Complete the following definitions of 'reverse' (Exercise 2.18) in terms of 'fold-right'
;and 'fold-left' from Exercise 2.38:
;---
;(define (reverse sequence)
;  (fold-right (lambda (x y) <??>) nil sequence))
;---
;(define (reverse sequence)
;  (fold-left (lambda (x y) <??>) nil sequence))
;------------------------------------------------------------------------------------------

;'add' -> helper
;---
(define (add elem lst)
  (if (null? lst)
      (cons elem nil)
      (cons (car lst)
            (add elem (cdr lst)))))

;'reverse-recur' -> recursive solution (needs 'add')
;---
(define (reverse-recur lst)
  (if (null? lst)
      nil
      (add (car lst)
           (reverse-recur (cdr lst)))))

;'reverse-iter' -> iterative solution
;---
(define (reverse-iter lst)
  (define (iter res rest)
    (if (null? rest)
        res
        (iter (cons (car rest) res)
              (cdr rest))))
  (iter nil lst))

;'fold-right' -> helper
;---
(define (fold-right op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (fold-right op init (cdr lst)))))

;'reverse-fr' -> in terms of 'fold-right' (similarities to 'reverse-recur')
;(also uses 'add')
;---
(define (reverse-fr lst)
  (fold-right add nil lst))

;'fold-left' -> helper
;---
(define (fold-left op init lst)
  (define (iter res rest)
    (if (null? rest)
        res
        (iter (op res (car rest))
              (cdr rest))))
  (iter init lst))

;'reverse-fl' -> in terms of 'fold-left' (similarities to 'reverse-iter')
;(because of the way 'fold-left' is implemented - the first argument of 'op' is 'res' and
;the second argument is (car rest) - we need to reverse the order of the arguments;
;otherwise, the result of reversing the list will be a series of nested lists)
;---
(define (reverse-fl lst)
  (fold-left (lambda (x y) (cons y x)) nil lst))

;test all implementations for random list
;---
(define lst (list 1 2 3 4))
(display "(reverse-recur lst) -> ") (reverse-recur lst)
(display "(reverse-iter lst) -> ") (reverse-iter lst)
(display "(reverse-fr lst) -> ") (reverse-fr lst)
(display "(reverse-fl lst) -> ") (reverse-fl lst)

