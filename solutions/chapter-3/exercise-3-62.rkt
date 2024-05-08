
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.62.
;---
;Use the results of Exercise 3.60 and Exercise 3.61 to define a procedure 'div-series'
;that divides two power series. 'div-series' should work for any two series, provided
;that the denominator series begins with a nonzero constant term. (If the denominator has
;a zero constant term, then div-series should signal an error.) Show how to use
;'div-series' together with the result of Exercise 3.59 to generate the power series for
;tangent.
;------------------------------------------------------------------------------------------

;'div-series'
;---
(define (div-series s1 s2)
  (let ((s2-const (stream-car s2))) ;get the denominator's constant term
    (if (= s2-const 0)
        (error "DIV-SERIES: Division by 0")
        (mul-series s1 (scale-stream ;restore the coefficients of 's2'
                        (invert-unit-series ;requires constant term to be 1 [3.61]
                         (scale-stream s2 (/ 1 s2-const)))
                        s2-const)))))

;'tangent-series'
;[definitions of 'sine-series' and 'cosine-series' in 3.59]
;---
(define tangent-series
  (div-series sine-series cosine-series))

