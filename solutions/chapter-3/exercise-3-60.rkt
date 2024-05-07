
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.60.
;---
;With power series represented as streams of coefficients as in Exercise 3.59, adding
;series is implemented by 'add-streams'. Complete the definition of the following
;procedure for multiplying series:
;---
;(define (mul-series s1 s2)
;  (cons-stream <??> (add-streams <??> <??>)))
;---
;You can test your procedure by verifying that sin^2(x) + cos^2(x) = 1, using the series
;from Exercise 3.59.
;------------------------------------------------------------------------------------------

;'add-series'
;---
(define add-series add-streams)
;---
(define (add-streams s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (cons-stream (+ (stream-car s1)
                         (stream-car s2))
                      (add-streams (stream-cdr s1)
                                   (stream-cdr s2))))))

;'mul-series'
;---
;The first element of the result stream is the product of the first elements of the input
;streams. To compute the remaining elements: 1. scale the second stream by the first
;element of the first stream; 2. compute the series product of the first stream's 'cdr'
;and the second stream; 3. add the two previously computed streams.
;---
;(1 2 3) * (4 5 6) = (4) + (5 6) + (2 3) * (4 5 6) =
;                  = (4) + (5 6) + (8) + (10 12) + (3) * (4 5 6) =
;                  = (4) + (5 6) + (8) + (10 12) + (12) + (15 18) + () * (4 5 6) =
;                  = (4) + (5 6) + (8) + (10 12) + (12) + (15 18) =
;                  = (4 5 6 8 10 12 12 15 18)
;---
;Note that the implementation suggested by the authors only works assuming that the first
;input stream is infinite. The program breaks if the first stream ever becomes null [if
;the first stream is null, 'mul-series' tries to calculate its 'stream-car' and fails,
;raising an error].
;---
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-series (scale-stream (stream-cdr s2) (stream-car s1))
                           (mul-series (stream-cdr s1) s2))))

