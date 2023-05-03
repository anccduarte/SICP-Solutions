
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.35.
;---
;Show that the golden ratio φ (Section 1.2.2) is a fixed point of the transformation
;x -> 1 + 1/x , and use this fact to compute φ by means of the 'fixed-point' (SEE
;BELOW *) procedure.
;------------------------------------------------------------------------------------------

;proof that φ is a fixed point of x -> 1 + 1/x
;---
;if x is a fixed point of f(x) = 1 + 1/x, then x = 1 + 1/x
;rewriting x, x = 1 + 1/x => x^2 = x + 1 => x^2 - x - 1 = 0
;so, x = 1 +/- (sqrt(1 - 4*1*(-1)) / 2*1) => x = 1 +/- sqrt(5)/2
;since φ = 1 + sqrt(5)/2, φ is a fixed-point of x -> 1 + 1/x
;hence, the proof is complete

;(*) 'fixed-point' procedure
;---
(define (fixed-point f guess)
  (define (close-enuf? old new)
    (< (abs (- new old)) 0.0001))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter guess (f guess)))

;compute φ by means of 'fixed-point'
;---
(let ((phi (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                         1.0)))
  phi)

