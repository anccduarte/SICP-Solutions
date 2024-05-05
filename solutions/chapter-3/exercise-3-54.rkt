
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.54.
;---
;Define a procedure 'mul-streams', analogous to 'add-streams', that produces the
;elementwise product of its two input streams. Use this together with the stream of
;integers to complete the following definition of the stream whose nth element (counting
;from 0) is n+1 factorial:
;---
;(define factorials
;  (cons-stream 1 (mul-streams <??> <??>)))
;------------------------------------------------------------------------------------------

;'add-streams'
;---
(define (add-streams s1 s2) (stream-map + s1 s2))

;'integers'
;---
(define ones (cons-stream 1 ones))
;---
(define integers (cons-stream 1
                              (add-streams ones integers)))

;'mul-streams'
;---
(define (mul-streams s1 s2) (stream-map * s1 s2))

;'factorials'
;---
(define factorials (cons-stream 1
                                (mul-streams factorials
                                             (stream-cdr integers))))
;---
;   1  2   6   24  120  ...  =  factorials
;   2  3   4    5    6  ...  =  (stream-cdr integers)
;1  2  6  24  120  720  ...  =  factorials

