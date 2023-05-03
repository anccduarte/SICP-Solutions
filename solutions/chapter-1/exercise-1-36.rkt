
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.36.
;---
;Modify 'fixed-point' so that it prints the sequence of approximations it generates,
;using the 'newline' and 'display' primitives shown in Exercise 1.22. Then find a
;solution to x^x = 1000 by finding a fixed point of x -> log(1000)/log(x). (Use Scheme's
;primitive 'log' procedure, which computes natural logarithms.) Compare the number of
;steps this takes with and without average damping. (Note that you cannot start
;'fixed-point' with a guess of 1, as this would cause division by log(1) = 0.)
;------------------------------------------------------------------------------------------

;defining procedure for displaying an approximation step
;---
(define (display-approx step old new)
  (display step)
  (display ". ")
  (display old)
  (display " ---> ")
  (display new)
  (newline))

;'fixed-point' printing the sequence of approximations it generates
;---
(define (fixed-point f guess verbose?)
  (define (close-enuf? old new)
    (< (abs (- new old)) 0.0001))
  (define (iter step old new)
    (if verbose?
        (display-approx step old new))
    (if (close-enuf? old new)
        new
        (iter (inc step) new (f new))))
  (iter 1 guess (f guess)))

;proof that finding x such that x^x = 1000 is equivalent to finding the fixed point of
;the function f(x) = log(1000)/log(x)
;---
;rewriting x^x = 1000
;x^x = 1000 => log(x^x) = log(1000) => x * log(x) = log(1000) => x = log(1000)/log(x)
;hence, x is a fixed-point of f(x) = log(1000)/log(x)
;so, the proof is complete

;defining 'average-damping'
;---
(define average (lambda (a b) (/ (+ a b) 2)))
;---
(define (average-damp f)
  (lambda (x) (average x (f x))))

;comparing number of steps with and without average damping
;---
(display "without average damping") (newline)
(display "---") (newline)
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0
             true)
;---
(newline) (display "with average damping") (newline)
(display "---") (newline)
(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x))))
             2.0
             true)

;conclusion
;---
;without average damping -> 29 steps
;with average damping -> 8 steps

