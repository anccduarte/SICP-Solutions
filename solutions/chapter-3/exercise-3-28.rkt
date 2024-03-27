
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.28.
;---
;Define an or-gate as a primitive function box. Your 'or-gate' constructor should be
;similar to 'and-gate'.
;------------------------------------------------------------------------------------------

;note on the construction of a language of circuits
;---
;"In essence, our simulator provides us with the tools to construct a language of
;circuits. If we adopt the general perspective on languages with which we approached the
;study of Lisp in Section 1.1, we can say that the primitive function boxes form the
;primitive elements of the language, that wiring boxes together provides a means of
;combination, and that specifying wiring patterns as procedures serves as a means of
;abstraction."

;'logical-or'
;---
(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signal(s)" (list s1 s2)))))

;'or-gate'
;---
(define (or-gate a1 a2 output)
  ;---
  (define (or-gate-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  ;---
  (add-action! a1 or-gate-procedure)
  (add-action! a2 or-gate-procedure)
  'ok)

