
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.18.
;---
;Consider an alternative strategy for scanning out definitions that translates the
;example in the text to [see below **]. Here 'a' and 'b' are meant to represent new
;variable names, created by the interpreter, that do not appear in the user's program.
;Consider the 'solve' procedure from Section 3.5.4:
;---
;(define (solve f y0 dt)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (stream-map f y))
;  y)
;---
;Will this procedure work if internal definitions are scanned out as shown in this
;exercise? What if they are scanned out as shown in the text [see below *]? Explain.
;------------------------------------------------------------------------------------------

;(*) Original transformation
;---
;Transforming 'solve' as originally suggested in the text is unproblematic. Setting 'y'
;to be the result of an integral whose integrand is '*unassigned* does not raise any
;issues because of its explicit delay. Moreover, by the time the "undelayed" 'dy' is set
;to a value directly dependent on 'y', the latter is already legally assigned. Hence, the
;trouble-free nature of the transformation.
;---
(lambda (vars)
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u 'e1)
    (set! v 'e2)
    'e3))
;---
(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

;(**) New transformation
;---
;The same does not hold for the validity of the newly suggested transformation. The error
;is easily catched: although there is no trouble in assigning 'a' to be the result of the
;integral [for the above-mentioned reasons], binding 'b' to (stream-map f y) is quite
;problematic. Since 'y' is set to '*unassigned* by the time such a binding occurs, the
;irrevocable constraint that 'stream-map' is applied to a stream [represented as a pair]
;is violated, thus making the present transformation inoperative.
;---
(lambda (vars)
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a 'e1)
          (b 'e2))
      (set! u a)
      (set! v b))
    'e3))
;---
(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

