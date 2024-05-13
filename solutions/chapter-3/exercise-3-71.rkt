
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.71.
;---
;Numbers that can be expressed as the sum of two cubes in more than one way are sometimes
;called Ramanujan numbers, in honor of the mathematician Srinivasa Ramanujan. Ordered
;streams of pairs provide an elegant solution to the problem of computing these numbers.
;To find a number that can be written as the sum of two cubes in two different ways, we
;need only generate the stream of pairs of integers (i, j) weighted according to the sum
;i^3 + j^3 (see Exercise 3.70), then search the stream for two consecutive pairs with the
;same weight. Write a procedure to generate the Ramanujan numbers. The first such number
;is 1,729. What are the next five?
;------------------------------------------------------------------------------------------

;auxiliary
;---
(define cube (lambda (x) (* x x x)))
(define sum-cubes (lambda (p) (+ (cube (car p)) (cube (cadr p)))))
;---
(define (move n s)
  (if (= n 0)
      s
      (move (- n 1) (stream-cdr s))))

;'ramanujan-numbers-fail'
;---
;The current solution actually returns the stream of all numbers that can be written as
;the sum of two cubes in two OR MORE different ways. If there is indeed a number [which
;there is] that can be written as the sum of two cubes in more than two different ways,
;that number will show up in the stream more than once; hence we have to perform an
;additional check: if we find out that the first two items of the stream have equivalent
;weights, we have to make sure that the third item has a distinct weight.
;---
(define (ramanujan-numbers-fail)
  (let ((sum-cubes-sorted
         (weighted-pairs integers integers sum-cubes)))
    (let lp ((curr (stream-car sum-cubes-sorted))
             (rest (stream-cdr sum-cubes-sorted)))
      (if (= (sum-cubes curr) (sum-cubes (stream-car rest)))
          (cons-stream (weight curr)
                       (lp (stream-car rest) (stream-cdr rest)))
          (lp (stream-car rest) (stream-cdr rest))))))

;'ramanujan-numbers'
;[first six Ramanujan numbers -> 1729, 4104, 13832, 20683, 32832, 39312]
;---
(define (ramanujan-numbers-fail)
  (let ((sum-cubes-sorted
         (weighted-pairs integers integers sum-cubes)))
    (let lp ((s sum-cubes-sorted))
      (let ((p1 (stream-car s))
            (p2 (stream-car (move 1 s)))
            (p3 (stream-car (move 2 s))))
      (if (= (sum-cubes p1) (sum-cubes p2))
          (if (not (= (sum-cubes p1) (sum-cubes p3)))
              (cons-stream (weight curr) (lp (move 2 s))) ;ignore first 2 pairs
              (lp (move 3 s)))                            ;ignore first 3 pairs
          (lp (move 1 s)))))))                            ;move to next pair in 's'

;'weighted-pairs'
;---
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;'merge-weighted'
;---
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1)
                                               s2
                                               weight)))
                 (else
                  (cons-stream s2car
                               (merge-weighted s1
                                               (stream-cdr s2)
                                               weight))))))))

