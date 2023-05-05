
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.40.
;---
;Define a procedure 'cubic' that can be used together with the 'newtons-method' procedure
;in expressions of the form
;(newtons-method (cubic a b c) 1)
;to approximate zeros of the cubic x^3 + ax^2 + bx + c.
;------------------------------------------------------------------------------------------


;---
;ON ABSTRACTIONS AND HIGHER-ORDER PROCEDURES
;---

;"In general, there are many ways to formulate a process as a procedure. Experienced
;programmers know how to choose procedural formulations that are particularly
;perspicuous, and where useful elements of the process are exposed as separate entities
;that can be reused in other applications."

;"As programmers, we should be alert to opportunities to identify the underlying
;abstractions in our programs and to build upon them and generalize them to create more
;powerful abstractions. This is not to say that one should always write programs in the
;most abstract way possible; expert programmers know how to choose the level of
;abstraction appropriate to their task."

;"In general, programming languages impose restrictions on the ways in which
;computational elements can be manipulated. Elements with the fewest restrictions are
;said to have first-class status. (...) Lisp, unlike other common programming languages,
;awards procedures full first-class status. This poses challenges for efficient
;implementation, but the resulting gain in expressive power is enormous."


;---
;ANSWER TO EXERCISE
;---

;'fixed-point' -> procedure that finds a fixed point of a function 'f' given an initial
;guess 'guess'
;---
(define (fixed-point f guess)
  (define (close-enuf? old new)
    (< (abs (- new old)) 0.00001))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter guess (f guess)))

;'deriv' -> computes the derivative of a function 'f' evalauted at 'x'
;f'(x) = (f(x+dx) - f(x)) / dx, for some small 'dx'
;---
(define (deriv f)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (f (+ x dx))
                      (f x))
                   dx))))

;'newton-transform' -> procedural representation of x - (f(x) / df(x))
;---
(define (newtons-transform f)
  (let ((df (deriv f)))
    (lambda (x) (- x
                   (/ (f x)
                      (df x))))))

;'newtons-method' -> computes the fixed point of the newton's transform on 'f'
;---
(define (newtons-method f)
  (fixed-point (newtons-transform f) 1.0))

;defining 'cubic'
;---
(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a (* x x))
                 (* b x)
                 c)))

;test 'newtons-method' on 'cubic'
;---
;real roots of x^3 + x^2 + 2x + 3 -> x = -1.27568
(newtons-method (cubic 1 2 3)) ;-1.2756822036498454
;---
;real roots of x^3 + 2x^2 + 3x + 1 -> x = -0.43016
(newtons-method (cubic 2 3 1)) ;-0.4301597090015873

