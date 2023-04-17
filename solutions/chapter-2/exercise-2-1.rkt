
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.1.
;---
;Define a better version of 'make-rat' that handles both positive and negative arguments.
;'make-rat' should normalize the sign so that if the rational number is positive, both
;the numerator and denominator are positive, and if the rational number is negative,
;only the numerator is negative.
;------------------------------------------------------------------------------------------

;euclid's gcd implementation
;---
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;if the denominator is negative, its sign will have to change whatever is the sign
;of the denominator. in this case, the sign of the numerator also changes. this part
;is taken care of in the consequent part of the if statement. when the signs are
;sorted just divide both the numerator and the denominator by the absolute value of 
;their greatest common divisor
;---
(define (make-rat n d)
  (if (< d 0)
      (make-rat (- n) (- d))
      (let ((g (abs (gcd n d))))
        (cons (/ n g) (/ d g)))))

;test for random rationals
;---
;positive numerator and denominator
(let ((a 2) (b 4))
  (make-rat a b))
;positive numerator and negative denominator
(let ((a 2) (b -4))
  (make-rat a b))
;negative numerator and positive denominator
(let ((a -2) (b 4))
  (make-rat a b))
;negative numerator and denominator
(let ((a -2) (b -4))
  (make-rat a b))

