
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.7.
;---
;The 'good-enough?' test used in computing square roots will not be very effective for
;finding the square roots of very small numbers. Also, in real computers, arithmetic
;operations are almost always performed with limited precision. This makes our test
;inadequate for very large numbers. Explain these statements, with examples showing how
;the test fails for small and large numbers. An alternative strategy for implementing
;'good-enough?' is to watch how guess changes from one iteration to the next and to stop
;when the change is a very small fraction of the guess. Design a 'square-root' procedure
;that uses this kind of end test. Does this work better for small and large numbers?
;------------------------------------------------------------------------------------------

;great answer by Maggyero (http://community.schemewiki.org/?sicp-ex-1.7)
;---
;PART 1
;"The initial strategy is to stop the improvement of the guess when the absolute error of
;the guess is less than a constant tolerance. For small radicands, the result is not
;accurate because the tolerance is not scaled down to the small radicands. For large
;radicands, the procedure 'sqrt-iter' enters an infinite recursion because the tolerance
;is not scaled up to the large radicands and floating-point numbers are represented with
;limited precision so the absolute error at that scale is always greater than the
;tolerance."
;---
;PART 2
;"An alternative strategy is to stop the improvement of the guess when the absolute error
;of the guess is less than a variable tolerance scaled to the radicand [that is, stop
;when (x-guess^2) < x*tolerance], in other words when the relative error of the guess is
;less than a constant tolerance [i.e., (x-guess^2)/x < tolerance]."
;---
;PART 3
;"Another alternative strategy is to stop the improvement of the guess when the absolute
;change of the guess is less than a variable tolerance scaled to the guess [that is, stop
;when improve(guess)-guess < guess*tol], in other words when the relative change of the
;guess is less than a constant tolerance [i.e., (improve(guess)-guess)/guess < tol]."

;implementation of 'sqrt' in terms of a variable procedure 'good-enuf?'
;---
(define (sqrt x good-enuf?)
  (define (improve guess) (/ (+ guess (/ x guess)) 2))
  (define (sqrt-iter guess)
    (if (good-enuf? guess x)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;implement 3 distinct versions of 'good-enuf?'
;---
;version 1 -> abs(x-guess^2) < tolerance
(define (good-enuf-1? g x)
  (define square (lambda (x) (* x x)))
  (define tolerance 0.00001)
  (< (abs (- x (square g))) tolerance))
;---
;version 2 -> abs((x-guess^2)/x) < tolerance
(define (good-enuf-2? g x)
  (define square (lambda (x) (* x x)))
  (define tolerance 0.00001)
  (< (abs (/ (- x (square g)) x)) tolerance))
;---
;version 3 -> abs((improve(guess)-guess)/guess) < tolerance
(define (good-enuf-3? g x)
  (define (improve gn) (/ (+ gn (/ x gn)) 2))
  (define tolerance 0.00001)
  (< (abs (/ (- (improve g) g) g)) tolerance))

;test 3 versions
;---
(define (test-sqrt good-enuf?)
  (display "sqrt(1e-100) = ") (display (sqrt 1e-100 good-enuf?)) (newline)
  (display "sqrt(4) = ") (display (sqrt 4 good-enuf?)) (newline)
  (display "sqrt(1e100) = ") (display (sqrt 1e100 good-enuf?)) (newline))
;---
;version 1
(display "version 1 ('god-enuf-1?')") (newline)
(display "do not run! '(sqrt 1e100)' enters infinite loop!") (newline)
;(test-sqrt good-enuf-1?)
;---
;version 2
(display "version 2 ('god-enuf-2?')") (newline)
(test-sqrt good-enuf-2?)
;---
;version 3
(display "version 3 ('god-enuf-3?')") (newline)
(test-sqrt good-enuf-2?)

