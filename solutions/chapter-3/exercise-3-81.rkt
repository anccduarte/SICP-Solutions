
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.81.
;---
;Exercise 3.6 discussed generalizing the random-number generator to allow one to reset
;the random-number sequence so as to produce repeatable sequences of "random" numbers.
;Produce a stream formulation of this same generator that operates on an input stream of
;requests to 'generate' a new random number or to 'reset' the sequence to a specified
;value and that produces the desired stream of random numbers. Don't use assignment in
;your solution.
;------------------------------------------------------------------------------------------

;helper procedures
;---
(define (generate? m) (eq? m 'generate))
;---
(define (reset? m)
  (and (pair? m)
       (eq? (car m) 'reset)
       (number? (cadr m))))

;'rand'
;[a valid 'requests' stream is assumed to be constituted by elements that are either the
;symbol 'generate or a list whose 'car' is the symbol 'reset and whose 'cadr' is a number]
;---
(define (rand requests)
  (let lp ((requests requests)
           (x random-init))
    (let* ((m (stream-car requests))
           (curr (cond ((generate? m) x)
                       ((reset? m) (cadr m))
                       (else (error "Unknown request" m)))))
      (cons-stream curr
                   (lp (stream-cdr requests)
                       (rand-update curr))))))

