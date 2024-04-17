
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.39.
;---
;Which of the five possibilities in the parallel execution shown above remain if we
;instead serialize execution as follows:
;---
;(define x 10)
;(define s (make-serializer))
;(parallel-execute
; (lambda () (set! x ((s (lambda () (* x x))))))
; (s (lambda () (set! x (+ x 1)))))
;------------------------------------------------------------------------------------------

;Assumptions
;---
;1. assume that serialized procedures in the same set may not execute simultaneously
;---
;2. assume that the first and second procedures which are arguments to 'parallel-execute'
;   are denoted by P1 and P2, respectively

;Valid possibilities
;---
;Important note: Possibility 4 may raise some doubts regarding its feasibility. However,
;a careful look into what actually happens dissipates any hesitation. The only operations
;that may not execute simultaneously are P1's double access to 'x' and P2's access and
;setting of 'x'. Once P1's access to 'x' is out of the way, all the remaining operations
;may execute in any given order (that is, it is not mandatory that P2's access and
;setting both happen either before or after P1's setting). Hence, P1's setting may be
;interleaved with P2's access and setting of 'x', resulting in the outcome exposed in 4.
;---
;1. P1 accesses 'x' twice (x=10, x=10)
;   P1 sets 'x' to 100
;   P2 accesses 'x' (x=100)
;   P2 increments 'x' to 101
;---
;2. P2 accesses 'x' (x=10)
;   P2 increments 'x' to 11
;   P1 accesses 'x' twice (x=11, x=11)
;   P1 sets 'x' to 121
;---
;3. P1 accesses 'x' twice (x=10, x=10)
;   P2 accesses 'x' (x=10)
;   P2 increments 'x' to 11
;   P1 sets 'x' to 100
;---
;4. P1 accesses 'x' twice (x=10, x=10)
;   P2 accesses 'x' (x=10)
;   P1 sets 'x' to 100
;   P2 increments 'x' to 11

