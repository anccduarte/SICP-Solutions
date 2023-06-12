
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.41.
;---
;Write a procedure to find all ordered triples of distinct positive integers 'i', 'j',
;and 'k' less than or equal to a given integer 'n' that sum to a given integer 's'.
;------------------------------------------------------------------------------------------

;helper procedures -> list operations
;---
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))
;---
(define (my-map proc lst)
  (if (null? lst)
      nil
      (cons (proc (car lst))
            (my-map proc (cdr lst)))))
;---
(define (filter predicate? lst)
  (cond ((null? lst) nil)
        ((predicate? (car lst)) (cons (car lst)
                                      (filter predicate? (cdr lst))))
        (else (filter predicate? (cdr lst)))))
;---
(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (accumulate op init (cdr lst)))))
;---
(define (flatmap proc seq)
  (accumulate append nil (my-map proc seq)))
;---
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))

;'ordered-triples' -> version 1
;(generates a list of triples (i j k) such that 1 <= k < j < i <= n)
;---
(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (my-map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;'ordered-triples-2' -> version 2
;(it may also be defined in terms of 'unique-pairs' from Exercise 2.40.; the idea is: for
;eaach k <= n, generate the pairs (i j) such that 1 <= j < i < k, and add 'k' to each one
;of these pairs -> avoids the extra nested mapping in 'ordered-triples')
;---
(define (unique-pairs n)
  (flatmap (lambda (i)
             (my-map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
;---
(define (ordered-triples-2 n)
  (flatmap (lambda (k)
             (my-map (lambda (pair)
                       (cons k pair))
                     (unique-pairs (- k 1))))
             (enumerate-interval 1 n)))

;'triple-sum'
;(verifies whether the sum of the elements of 'lst' is equal to 's')
;---
(define (triple-sum? lst s)
  (= (accumulate + 0 lst) s))

;'triple-sum-target'
;(returns a list of triples (i j k) such that 1 <= k < j < i <= n, and the sum of the
;elements in each of the returned triples is equal to 's')
;----
(define (triple-sum-target n s)
  (filter (lambda (triple)
            (triple-sum? triple s))
          (ordered-triples n)))

;test for random values of n and s
;---
(define (test n s)
  (display "(ordered-triples ") (display n) (display ") -> ")
  (display (ordered-triples n)) (newline)
  (display "(triple-sum-target ") (display n) (display " ")(display s) (display ") -> ")
  (display (triple-sum-target n s)) (newline))
;---
(test 4 8)
(display "---") (newline)
(test 5 8)

