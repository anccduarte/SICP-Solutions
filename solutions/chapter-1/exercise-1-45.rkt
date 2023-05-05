
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.45.
;---
;We saw in Section 1.3.3 that attempting to compute square roots by naively finding a
;fixed point of y -> x/y does not converge, and that this can be fixed by average
;damping. The same method works for finding cube roots as fixed points of the
;average-damped y -> x/y^2. Unfortunately, the process does not work for fourth roots — a
;single average damp is not enough to make a fixed-point search for y -> x/y^3 converge.
;On the other hand, if we average damp twice (i.e., use the average damp of the average
;damp of y -> x/y^3) the fixed-point search does converge. Do some experiments to
;determine how many average damps are required to compute nth roots as a fixed-point
;search based upon repeated average damping of y -> x/y^(n-1). Use this to implement a
;simple procedure for computing nth roots using 'fixed-point', 'average-damp', and the
;'repeated' procedure of Exercise 1.43. Assume that any arithmetic operations you need
;are available as primitives.
;------------------------------------------------------------------------------------------


;---
;PART 1
;(defining procedures)
;---

;defining 'fixed-point'
;---
(define (fixed-point f guess)
  (define (close-enuf? old new)
    (let ((tolerance 0.00001))
      (< (abs (- new old)) tolerance)))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter guess (f guess)))

;defining 'average-damp'
;---
(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

;defining 'repetead'
;---
(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))


;---
;PART 2
;(testing the amount of necessary average damps for nth roots)
;---

;'fast-exp' -> helper procedure (computes b^e)
;---
(define (fast-exp b e)
  (define even?
    (lambda (x) (= (remainder x 2) 0)))
  (define square
    (lambda (x) (* x x)))
  (cond ((= e 0) 1)
        ((even? e) (square (fast-exp b (/ e 2))))
        (else (* b (fast-exp b (dec e))))))

;testing for roots from 2nd to 32th degree
;---
(display "Roots in terms of 'fixed-point'") (newline)
(display "---") (newline)
;---
(define (root-2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
;'root-2' works with ONE average damp
(root-2 4)
;---
(define (root-3 x)
  (fixed-point (average-damp (lambda (y) (/ x (fast-exp y 2))))
               1.0))
;'root-3' works with ONE average damp
(root-3 8)
;---
(define (root-4 x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (fast-exp y 3))))
               1.0))
;'root-4' works with TWO average damps
(root-4 16)
;---
(define (root-5 x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (fast-exp y 4))))
               1.0))
;'root-5' works with TWO average damps
(root-5 32)
;---
(define (root-6 x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (fast-exp y 5))))
               1.0))
;'root-6' works with TWO average damps
(root-6 64)
;---
(define (root-7 x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (fast-exp y 6))))
               1.0))
;'root-7' works with TWO average damps
(root-7 128)
;---
(define (root-8 x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (fast-exp y 7))))
               1.0))
;'root-8' works with THREE average damps
(root-8 256)
;---
(define (root-9 x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (fast-exp y 8))))
               1.0))
;'root-9' works with THREE average damps
(root-9 512)
;---
(define (root-15 x)
  (fixed-point ((repeated average-damp 3) (lambda (y) (/ x (fast-exp y 14))))
               1.0))
;'root-15' works with THREE average damps
(root-15 32768)
;---
(define (root-16 x)
  (fixed-point ((repeated average-damp 4) (lambda (y) (/ x (fast-exp y 15))))
               1.0))
;'root-16' works with FOUR average damps
(root-16 65536)
;---
(define (root-31 x)
  (fixed-point ((repeated average-damp 4) (lambda (y) (/ x (fast-exp y 30))))
               1.0))
;'root-31' works with FOUR average damps
(root-31 2147483648)
;---
(define (root-32 x)
  (fixed-point ((repeated average-damp 5) (lambda (y) (/ x (fast-exp y 31))))
               1.0))
;'root-32' works with FIVE average damps
(root-32 4294967296)


;---
;PART 3
;(uncovering the pattern and abstracting it into a procedure)
;---

;uncovering the pattern
;---
;n ∈ [2,3] => 1 average damp
;n ∈ [4,7] => 2 average damps
;n ∈ [8,15] => 3 average damps
;n ∈ [16,31] => 4 average damps
;n ∈ [32,63] => 5 average damps
;...

;determining the rule which gives rise to the pattern
;---
;2^(1+1) > n (∀ n ∈ [2,3]) => 2^(1+1) > 3 => 4 > 3 (#t)
;2^(2+1) > n (∀ n ∈ [4,7]) => 2^(2+1) > 7 => 8 > 7 (#t)
;2^(3+1) > n (∀ n ∈ [8,15]) => 2^(3+1) > 15 => 16 > 15 (#t)
;2^(4+1) > n (∀ n ∈ [16,31]) => 2^(4+1) > 31 => 32 > 31 (#t)
;2^(5+1) > n (∀ n ∈ [32,63]) => 2^(5+1) > 63 => 64 > 63 (#t)
;---
;hence, the rule for determining the number of necessary average damps may be given by
;the expression:
;2^(num-av-damps + 1) > n => log2(2^(num-av-damps + 1)) > log2(n) =>
;                         => num-av-damps + 1 > log2(n) => num-av-damps > log2(n) - 1 =>
;                         => num-av-damps = ceil(log2(n)-1)

;abstracting the rule into a procedure
;---
;(ceiling n) -> the closest integer to n, which is larger than or equal to n
;(log x b) -> log(x) base b
;---
;(*) a very small quantity is added to t:=(- (log n 2) 1) in (+ (- (log n 2) 1) 0.0001)
;to account for the cases when (log n 2) results in an integer (i.e., when n is a power
;of 2). in these cases, 'ceiling' does not round 't' to its closest larger integer, hence
;the need to add such a small quantitity to 't'. for example:
;2^(1+1) > 2 => 1+1 > log2(2) => 1 > log2(2)-1 => 1 > 0 => 1 = ceil(0) => 1 = 0 (#f)
;2^(1+1) > 2 => 1+1 > log2(2) => 1 > log2(2)-1 => 1 > 0 => 1 = ceil(0.0001) => 1 = 1 (#t)
;---
(define (root-n x n)
  (let ((num (ceiling (+ (- (log n 2) 1) 0.0001)))) ;(*)
    (fixed-point ((repeated average-damp num) (lambda (y)
                                                (/ x (fast-exp y (dec n)))))
                 1.0)))

;testing for roots from 2nd to 32th degree
;---
(newline) (display "Roots in terms of 'root-n'") (newline)
(display "---") (newline)
;---
(root-n 4 2)
(root-n 8 3)
(root-n 16 4)
(root-n 32 5)
(root-n 64 6)
(root-n 128 7)
(root-n 256 8)
(root-n 512 9)
(root-n 32768 15)
(root-n 65536 16)
(root-n 2147483648 31)
(root-n 4294967296 32)

