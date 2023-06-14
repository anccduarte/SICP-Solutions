
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.47.
;---
;Here are two possible constructors for frames:
;---
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;---
;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;---
;For each constructor supply the appropriate selectors to produce an implementation for
;frames.
;------------------------------------------------------------------------------------------

;constructor 1 -> 'make-frame-1'
;---
(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

;selectors for 'make-frame-1'
;---
(define (origin-frame-1 frame)
  (car frame))
;---
(define (edge1-frame-1 frame)
  (car (cdr frame)))
;---
(define (edge2-frame-1 frame)
  (car (cdr (cdr frame))))

;constructor 2 -> 'make-frame-2'
;---
(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;selectors for 'make-frame-2'
;---
(define (origin-frame-2 frame)
  (car frame))
;---
(define (edge1-frame-2 frame)
  (car (cdr frame)))
;---
(define (edge2-frame-2 frame)
  (cdr (cdr frame)))

;tests for random frame
;---
(define frame-1 (make-frame-1 (cons 0 0)
                              (cons 1 2)
                              (cons 3 4)))
;---
(define frame-2 (make-frame-2 (cons 0 0)
                              (cons 1 2)
                              (cons 3 4)))
;---
(display "CONSTRUCTOR 1") (newline)
(display "origin -> ") (origin-frame-1 frame-1)
(display "edge1 -> ") (edge1-frame-1 frame-1)
(display "edge2 -> ") (edge2-frame-1 frame-1)
;---
(newline) (display "CONSTRUCTOR 2") (newline)
(display "origin -> ") (origin-frame-2 frame-2)
(display "edge1 -> ") (edge1-frame-2 frame-2)
(display "edge2 -> ") (edge2-frame-2 frame-2)

