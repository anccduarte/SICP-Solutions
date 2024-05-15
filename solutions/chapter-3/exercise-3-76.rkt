
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.76.
;---
;Eva Lu Ator has a criticism of Louis's approach in Exercise 3.75. The program he wrote
;is not modular, because it intermixes the operation of smoothing with the zero-crossing
;extraction. For example, the extractor should not have to be changed if Alyssa finds a
;better way to condition her input signal. Help Louis by writing a procedure 'smooth'
;that takes a stream as input and produces a stream in which each element is the average
;of two successive input stream elements. Then use 'smooth' as a component to implement
;the zero-crossing detector in a more modular style.
;------------------------------------------------------------------------------------------

;'smooth'
;---
(define (smooth input-stream)
  (stream-map (lambda (x y) (/ (+ x y) 2))
              input-stream
              (stream-cdr input-stream)))

;'make-zero-crossing' [without using 'stream-map']
;---
(define (make-zero-crossing s)
  (let ((smoothed (smooth s)))
    (let lp ((s (stream-cdr smoothed))
             (last (stream-car smoothed)))
    (cons-stream
     (sign-change-detector (stream-car s) last)
     (lp (stream-cdr s) (stream-car s))))))

;'make-zero-crossing' [exploiting 'stream-map']
;---
(define (make-zero-crossing s)
  (let ((smoothed-s (smooth s)))
    (stream-map sign-change-detector
                (stream-cdr smoothed-s)
                smoothed-s)))
  
