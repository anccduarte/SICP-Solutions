
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.35.
;---
;Ben Bitdiddle tells Louis that one way to avoid the trouble in Exercise 3.34 is to
;define a squarer as a new primitive constraint. Fill in the missing portions in Ben's
;outline for a procedure to implement such a constraint:
;---
;(define (squarer a b)
;  (define (process-new-value)
;    (if (has-value? b)
;        (if (< (get-value b) 0)
;            (error "square less than 0: SQUARER"
;                   (get-value b))
;            <alternative1>)
;        <alternative2>))
;  (define (process-forget-value) <body1>)
;  (define (me request) <body2>)
;  <rest of definition>
;  me)
;------------------------------------------------------------------------------------------

;'squarer'
;[the procedure is to be integrated in the constant propagation system; for the complete
;implementation of the system, see "extra-3-constraints.rkt"]
;---
(define (squarer a b)
  ;---
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b) 0)
               (error "square less than 0: SQUARER" (get-value b))
               (set-value! a (sqrt (get-value b)) me)))
          ((has-value? a)
           (set-value! b (* (get-value a) (get-value a)) me))))
  ;---
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  ;---
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  ;---
  (connect a me)
  (connect b me)
  me)

;hypothetical test
;---
(let ((a (make-connector))
      (b (make-connector)))
  ;---
  (squarer a b)
  ;---
  (probe 'a a)
  (probe 'b b)
  ;---
  (set-value! a 2 'user)
  (display "---") (newline)
  (forget-value! a 'user)
  (display "---") (newline)
  (set-value! b 4 'user)
  (display ""))

