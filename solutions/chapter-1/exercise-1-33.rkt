
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.33.
;---
;You can obtain an even more general version of 'accumulate' (Exercise 1.32) by
;introducing the notion of a filter on the terms to be combined. That is, combine only
;those terms derived from values in the range that satisfy a specified condition. The
;resulting 'filtered-accumulate' abstraction takes the same arguments as 'accumulate',
;together with an additional predicate of one argument that specifies the filter. Write
;'filtered-accumulate' as a procedure. Show how to express the following using
;'filtered-accumulate':
;---
;a. the sum of the squares of the prime numbers in the interval 'a' to 'b' (assuming that
;you have a 'prime?' predicate already written).
;---
;b. the product of all the positive integers less than 'n' that are relatively prime to
;'n' (i.e., all positive integers i < n such that gcd(i, n) = 1).
;------------------------------------------------------------------------------------------


;---
;COMMON TO PARTS A AND B
;---

;defining 'filtered-accumulate'
;---
(define (filtered-accumulate predicate? combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (predicate? a) (term a) null-value)
                (filtered-accumulate predicate?
                                     combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b))))


;---
;PART A
;---

;defining 'prime?'
;---
(define square (lambda (x) (* x x)))
;---
(define (divides? a b)
  (= (remainder a b) 0))
;---
(define (smallest-divisor n)
  (define (iter count)
    (cond ((> (square count) n) n)
          ((divides? n count) count)
          (else (iter (inc count)))))
  (iter 2))
;---
(define (prime? n)
  (= (smallest-divisor n) n))

;ANSWER to part (a)
;---
(define (sum-squares-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

;test 'sum-squares-primes'
;---
(display "PART A") (newline)
;1 + 4 + 9 + 25 + 49 = 88
(display "[1,10] -> ") (sum-squares-primes 1 10)
;1 + 4 + 9 + 25 + 49 + 121 + 169 + 289 + 361 = 1028
(display "[1,20] -> ") (sum-squares-primes 1 20)


;---
;PART B
;---

;defining 'gcd'
;---
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;ANSWER to part (b)
;'relatively-prime?' must be written inside of 'prod-rel-prime' as 'predicate?' can only
;take one argument
;---
(define (prod-rel-prime n)
  (filtered-accumulate (lambda (i) (= (gcd i n) 1))
                       *
                       1
                       (lambda (x) x)
                       1
                       inc
                       (dec n)))

;test 'prod-rel-prime'
;---
(newline) (display "PART B") (newline)
;1 * 3 * 7 * 9 = 189
(display "[1,10] -> ") (prod-rel-prime 10)
;1 * 3 * 7 * 9 * 11 * 13 * 17 * 19 = 8729721
(display "[1,20] -> ") (prod-rel-prime 20)

