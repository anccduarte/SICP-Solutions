
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.65.
;---
;Use the series
;---
;ln(2) = 1 - 1/2 + 1/3 - 1/4 + ...
;---
;to compute three sequences of approximations to the natural logarithm of 2, in the same
;way we did above for pi. How rapidly do these sequences converge?
;------------------------------------------------------------------------------------------

;'ln2-stream'
;---
(define ln2-stream
  (partial-sums (ln2-summands 1)))
;---
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

;'accelerated-ln2-stream'
;---
;                    ln(2) -> 0.6931471806 [actual result]
;(stream-ref ln2-stream 0) -> 1.0000000000 [1st iteration]
;(stream-ref ln2-stream 1) -> 0.5000000000 [2nd iteration]
;(stream-ref ln2-stream 2) -> 0.8333333333 [3rd iteration]
;(stream-ref ln2-stream 3) -> 0.5833333333 [4th iteration]
;(stream-ref ln2-stream 4) -> 0.7833333333 [5th iteration]
;(stream-ref ln2-stream 5) -> 0.6166666667 [6th iteration]
;(stream-ref ln2-stream 6) -> 0.7595238095 [7th iteration]
;(stream-ref ln2-stream 7) -> 0.6345238095 [8th iteration]
;(stream-ref ln2-stream 8) -> 0.7456349206 [9th iteration]
;---
;Clearly, these sequences converge rather slowly. As demonstrated for pi, an accelaration
;mechanism may be used for achieving convergence more expeditiously [see below].
;---
(define accelerated-ln2-stream
  (accelerated-sequence euler-transform ln2-stream))
;---
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
;---
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
;---
(define (euler-transform transform s)
  (let ((s0 (stream-ref s 0))  ; S(n-1)
        (s1 (stream-ref s 1))  ; S(n)
        (s2 (stream-ref s 2))) ; S(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

