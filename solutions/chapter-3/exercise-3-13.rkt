
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.13.
;---
;Consider the following 'make-cycle' procedure, which uses the 'last-pair' procedure
;defined in Exercise 3.12:
;---
;(define (make-cycle x)
;  (set-cdr! (last-pair x) x)
;  x)
;---
;Draw a box-and-pointer diagram that shows the structure 'z' created by
;---
;(define z (make-cycle (list 'a 'b 'c)))
;---
;What happens if we try to compute '(last-pair z)'?
;------------------------------------------------------------------------------------------

;'MAKE-CYCLE'
;---
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
;---
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;TEST
;---
(define x '(a b c))
(define z (make-cycle x)) z
;(last-pair z) -> produces an infinite loop (see diagram below); moreover 'x' is mutated,
;                 hence, (last-pair x) also generates an infinite loop

;DIAGRAM
;---
; x ---> +---+---+     +---+---+     +---+---+
; z ---> | | | ------> | | | ------> | | | -------+
; +----> +-|-+---+     +-|-+---+     +-|-+---+    |
; |        ↓             ↓             ↓          |
; |       'a            'b            'c          |
; +-----------------------------------------------+

