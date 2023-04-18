
#lang sicp

;------------------------------------------------------------------------------------------
;HERON'S SQRT
;---
;a number y is the square root of a number x if y^2 = x. rearranging the previous
;equation, we get y = x/y. hence, we may find the square root y of x by iterating the
;function x/y until y is close enough to x/y (at this point, y is a fixed-point of x/y).
;however, finding the fixed-point of x/y does not yield any useful result. in fact, if we
;try to find the fixed-point of such a function, we enter an infinite loop. consider the
;case of finding the square root of 2 (x) and let 1 be the initial guess for the square
;root of 2 (y). in the first iteration, we get y = 2/1 = 2. in the second iteration, we
;get y = 2/2 = 1. in the third iteration, we get 2/1 = 2. this pattern repeats
;indefinitly, since the absolute value of the difference between y and x/y is always 1 (a
;fairly large value when compared to the tolerances used in this kind of procedures). to
;solve this problem, a technique called average damping is utilized. here, instead of
;attributing x/y to the new value of y, yn, yn is assigned to the average of the previous
;value of y and x/y, that is, yn = (y + x/y) / 2. hence, to find the square root of a
;given number x, one must simply find the fixed-point of the function (y + x/y) / 2.
;------------------------------------------------------------------------------------------

;the square root of x is the fixed-point of (y + x/y) / 2 
;---
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;'average-damp' takes a procedure 'f' as input and returns a procedure of one argument
;'x' that averages 'x' and the result of applying 'f' to 'x'
;---
(define average (lambda (x y) (/ (+ x y) 2)))
(define (average-damp f)
  (lambda (x) (average x (f x))))

;'fixed-point' takes as inputs a procedure 'f' (representing the function whose
;fixed-point is to be found) and an initial guess 'guess'
;---
(define (fixed-point f guess)
  (define (close-enuf? new old)
    (< (abs (- old new)) 0.00001))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter guess (f guess)))

;testing 'sqrt' for random numbers
;---
(define (display-sqrt x)
  (display "sqrt(") (display x) (display ") = ")
  (display (sqrt x)) (newline))
;---
(display-sqrt 1)
(display-sqrt 2)
(display-sqrt 3)
(display-sqrt 4)

;note: determining if two consecutive guesses y(n) and y(n+1) are close enough by simply
;checking whether the absolute value of the difference between y(n) and y(n+1) lies
;whithin a range specified by a tolerance (e.g., tol=0.00001) might become problematic as
;we compute the square roots of very small numbers, namely numbers smaller than the
;specified tolerance. in such cases, although abs(y(n+1)-y(n)) might be smaller than the
;tolerance, we inevitably end up facing precision issues ("it is like asking to measure
;the size of a coin, plus or minus one meter; [although] the result can be technically
;correct, it is still not very useful.")

