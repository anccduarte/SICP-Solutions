
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.64.
;---
;Write a procedure 'stream-limit' that takes as arguments a stream and a number (the
;tolerance). It should examine the stream until it finds two successive elements that
;differ in absolute value by less than the tolerance, and return the second of the two
;elements. Using this, we could compute square roots up to a given tolerance by
;---
;(define (sqrt x tolerance)
;  (stream-limit (sqrt-stream x) tolerance))
;------------------------------------------------------------------------------------------

;'stream-limit'
;---
(define (stream-limit s tolerance)
  ;---
  (define good-enough?
    (lambda (a b) (< (abs (- a b)) tolerance)))
  ;---
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (good-enough? s0 s1)
        s1
        (stream-limit (stream-cdr s) tolerance))))

