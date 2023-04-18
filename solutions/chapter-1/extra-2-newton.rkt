
#lang sicp

;------------------------------------------------------------------------------------------
;NEWTON'S METHOD
;---
;given a function 'f', its derivative 'df' and a guess 'x', the newton's method finds the
;root(s) of 'f' (i.e., f=0) by determining the fixed-point of x - (f(x) / df(x)). thus,
;new guesses are iteratively produced by doing x2 = x1 - (f(x1) / df(x1)), which is
;algebraically equivalent to x1 - x2 = f(x1) / df(x1). so, whenever abs(x1-x2)~=0 (i.e.,
;x1 (~=x2) is a fixed-point of the function), then f(x1) has to be ~=0. hence, x1 is a
;root of 'f'.
;------------------------------------------------------------------------------------------

;'sqrt' -> finds the square root of a given number 'x' by newton's method (calls 'newton'
;on (x - y^2); since x is given, it finds a y such that f = x - y^2 = 0)
;---
(define square (lambda (x) (* x x)))
(define (sqrt x)
  (newton (lambda (y) (- x (square y)))))

;'newton' calls 'fixed-point' on x - (f(x) / df(x))
;---
(define (newton f)
  (define df (deriv f))
  (fixed-point (lambda (x) (- x
                              (/ (f x) (df x))))
               1.0))

;'deriv' computes (f(x+dx) - f(x)) / dx
;---
(define (deriv f)
  (define dx 0.00001)
  (lambda (x) (/ (- (f (+ x dx)) (f x))
                 dx)))

;'fixed-point' -> while abs((y2:=f(y1))-y1)>=tol, y1=y2 and y2=f(y2)
;---
(define (fixed-point f guess)
  (define (close-enuf? old new)
    (< (abs (- new old)) 0.0001))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter guess (f guess)))

;test 'sqrt' for random numbers
;---
(define (display-sqrt x)
  (display "sqrt(") (display x) (display ") = ")
  (display (sqrt x)) (newline))
;---
(display-sqrt 1)
(display-sqrt 2)
(display-sqrt 3)
(display-sqrt 4)

