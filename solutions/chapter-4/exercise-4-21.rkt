
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.21.
;---
;Amazingly, Louis's intuition in Exercise 4.20 is correct. It is indeed possible to
;specify recursive procedures without using 'letrec' (or even 'define'), although the
;method for accomplishing this is much more subtle than Louis imagined. The following
;expression computes 10 factorial by applying a recursive factorial procedure [see
;footnote below *]:
;---
;((lambda (n)
;   ((lambda (fact) (fact fact n))
;    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
; 10)
;---
;(a) Check (by evaluating the expression) that this really does compute factorials.
;    Devise an analogous expression for computing Fibonacci numbers.
;---
;(b) Consider the following procedure, which includes mutually recursive internal
;    definitions:
;    ---
;    (define (f x)
;      (define (even? n)
;        (if (= n 0) true (odd? (- n 1))))
;      (define (odd? n)
;        (if (= n 0) false (even? (- n 1))))
;      (even? x))
;    ---
;    Fill in the missing expressions to complete an alternative definition of 'f', which
;    uses neither internal definitions nor 'letrec':
;    ---
;    (define (f x)
;      ((lambda (even? odd?) (even? even? odd? x))
;       (lambda (ev? od? n)
;         (if (= n 0) true (od? <??> <??> <??>)))
;       (lambda (ev? od? n)
;         (if (= n 0) false (ev? <??> <??> <??>)))))
;------------------------------------------------------------------------------------------

;(*) footnote 27
;---
;"This example illustrates a programming trick for formulating recursive procedures
;without using 'define'. The most general trick of this sort is the Y operator, which can
;be used to give a "pure λ-calculus" implementation of recursion."

;(a) evaluating 4 factorial
;---
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 4)
;---
((lambda (fact) (fact fact 4))
 (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))
;---
((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
 (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
 4)
;---
(* 4
   ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    3))
;---
(* 4
   3
   ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    2))
;---
(* 4
   3
   2
   ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    1))
;---
(* 4 3 2 1)
;---
24

;(a) computing Fibonacci numbers [from 0 to 8]
;---
(map (lambda (n)
       ((lambda (fibo) (fibo fibo n))
        (lambda (fb k) (if (< k 2)
                           k
                           (+ (fb fb (- k 1))
                              (fb fb (- k 2)))))))
     '(0 1 2 3 4 5 6 7 8))

;(b) filling in the blanks and evaluating 'f'
;[alternates the application of 'even?' and 'odd?' to the actual argument 'x' (which is
;"updated" as the program runs), while keeping track of the code for 'even?' and 'odd?'
;for reference in subsequent applications; note that, as specified by the original 'f',
;the program starts by verifying whether the argument to 'f' is even]
;---
(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
;---
(map f '(0 1 2 3 4 5 6 7 8))

;(b) evaluating (f 4)
;---
(f 4)
;---
((lambda (even? odd?) (even? even? odd? 4))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1)))))
;---
((lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 4)
;---
((lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 3)
;---
((lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 2)
;---
((lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 1)
;---
((lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) true (od? ev? od? (- n 1))))
 (lambda (ev? od? n)
   (if (= n 0) false (ev? ev? od? (- n 1))))
 0)
;---
true

