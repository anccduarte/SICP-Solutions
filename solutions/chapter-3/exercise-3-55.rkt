
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.55.
;---
;Define a procedure 'partial-sums' that takes as argument a stream 'S' and returns the
;stream whose elements are S0, S0+S1, S0+S1+S2, ... For example, (partial-sums integers)
;should be the stream 1, 3, 6, 10, 15, ...
;------------------------------------------------------------------------------------------

;'partial-sums'
;---
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;(partial-sums integers)
;---
;   1  3   6  10  15  21  ...  =  (partial-sums integers)
;   2  3   4   5   6   7  ...  =  (stream-cdr integers)
;1  3  6  10  15  21  28  ...  =  (partial-sums integers)

;alternative 'partial-sums'
;[very elegant implementation at http://community.schemewiki.org/?sicp-ex-3.55]
;---
(define (partial-sums s)
  (add-streams s
               (cons-stream 0
                            (partial-sums s))))

;(partial-sums integers)
;---
;   1  3   6  10  15  21  ...  =  integers
;   2  3   4   5   6   7  ...  =  (cons-stream 0 (partial-sums integers))
;1  3  6  10  15  21  28  ...  =  (partial-sums integers)

