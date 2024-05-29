
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.78.
;---
;Consider the problem of designing a signal-processing system to study the homogeneous
;second-order linear differential equation
;---
;d^2(y)/dt^2 - a(dy/dt) - by = 0
;---
;The output stream, modeling y, is generated by a network that contains a loop. This is
;because the value of d^2(y)/dt^2 depends upon the values of y and dy/dt and both of
;these are determined by integrating d^2(y)/dt^2. The diagram we would like to encode is
;shown in Figure 3.35 [see in book]. Write a procedure 'solve-2nd' that takes as
;arguments the constants a, b, and dt and the initial values y0 and dy0 for y and dy/dt
;and generates the stream of successive values of y.
;------------------------------------------------------------------------------------------

;'solve-2nd'
;[exact same logic as in 'solve' (see book)]
;---
(define (sove-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream y b)
                           (scale-stream dy a)))
  y)

;'integral'
;---
(define (integral delayed-integrand init-value dt)
  (define int
    (cons-stream
     init-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
                    int))))
  int)
