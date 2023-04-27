
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.17.
;---
;The exponentiation algorithms in this section are based on performing exponentiation by
;means of repeated multiplication. In a similar way, one can perform integer
;multiplication by means of repeated addition. The following multiplication procedure (in
;which it is assumed that our language can only add, not multiply) is analogous to the
;'expt' procedure (SEE BELOW *). This algorithm takes a number of steps that is linear in
;'b'. Now suppose we include, together with addition, operations 'double', which doubles
;an integer, and 'halve', which divides an (even) integer by 2. Using these, design a
;multiplication procedure analogous to 'fast-expt' that uses a logarithmic number of
;steps.
;------------------------------------------------------------------------------------------

;(*) procedure for multiplying two number a and b
;---
(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (dec b)))))

;'mult' procedure analogous to 'fast-exp'
;if b is even => a*b = 2*(a*(b/2))
;if b is odd => a*b = a+(a*(b-1))
;---
(define double (lambda (x) (* x 2)))
(define halve (lambda (x) (/ x 2)))
(define even? (lambda (x) (= (remainder x 2) 0)))
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (dec b))))))

