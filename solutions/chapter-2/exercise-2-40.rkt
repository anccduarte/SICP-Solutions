
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.40.
;---
;Define a procedure 'unique-pairs' that, given an integer 'n', generates the sequence of
;pairs (i, j) with 1 <= j < i <= n. Use 'unique-pairs' to simplify the definition of
;'prime-sum-pairs' given above.
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
  (if (null? lst)
      nil
      (let ((head (car lst))
            (next (filter predicate? (cdr lst))))
        (if (not (predicate? head))
            next
            (cons head next)))))
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

;helper procedures -> testing for primality
;---
(define (prime? n)
  (define square
    (lambda (x) (* x x)))
  (define (divides? a b)
    (= (remainder a b) 0))
  (define (smallest-divisor x)
    (define (iter y)
      (cond ((> (square y) x) x)
            ((divides? x y) y)
            (else (iter (+ y 1)))))
    (iter 2))
  (= (smallest-divisor n) n))
;---
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;'unique-pairs'
;---
(define (unique-pairs n)
  (flatmap (lambda (i)
             (my-map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;'make-pair-sum'
;---
(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

;redefining 'prime-sum-pairs' in terms of 'unique-pairs'
;---
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;test for random n
;---
(prime-sum-pairs 6)

