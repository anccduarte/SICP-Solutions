
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.20.
;---
;Draw environment diagrams to illustrate the evaluation of the sequence of expressions
;---
;(define x (cons 1 2))
;(define z (cons x x))
;(set-car! (cdr z) 17)
;(car x) -> 17
;---
;using the procedural implementation of pairs given above [see definitions below].
;(Compare Exercise 3.11.)
;------------------------------------------------------------------------------------------

;note on assignment and mutability
;---
;"Assignment is all that is needed, theoretically, to account for the behavior of mutable
;data. As soon as we admit 'set!' to our language, we raise all the issues, not only of
;assignment, but of mutable data in general."

;defining 'cons'
;---
(define (cons x y)
  ;---
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  ;---
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else
           (error "Undefined operation: CONS" m))))
  ;---
  dispatch)

;defining 'car', 'cdr', 'set-car!' and 'set-cdr!'
;---
(define (car x) (x 'car))
;---
(define (cdr x) (x 'cdr))
;---
(define (set-car! x new-value)
  ((x 'set-car!) new-value) x)
;---
(define (set-cdr! x new-value)
  ((x 'set-cdr!) new-value) x)

;defining the sequence of expressions
;(for an awesome drawing of the environment diagrams generated by the evaluation of such
;expressions refer to codybartfast's solution -> community.schemewiki.org/?sicp-ex-3.20;
;note that codybartfast's solution does not assume 'cons', 'set-car!', 'cdr' and 'car' to
;be primitive procedures; supposing that such procedures are primitives makes the task of
;drawing these diagrams musch easier [see simplified version of the diagrams below])
;---
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
;---
;                                   1   2
;                                   ↑   ↑
;             +----------+        +-|-+-|-+
; global ---> | x: -------------> | | | | |   [before 'set-car!']
;    env      | y: --+   |        +---+---+
;             +------|---+          ↑   ↑
;                    |              |   |
;                    |            +-|-+-|-+
;                    +----------> | | | | |
;                                 +---+---+
;---
;                                  17   2
;                                   ↑   ↑
;             +----------+        +-|-+-|-+
; global ---> | x: -------------> | | | | |   [after 'set-car!']
;    env      | y: --+   |        +---+---+
;             +------|---+          ↑   ↑
;                    |              |   |
;                    |            +-|-+-|-+
;                    +----------> | | | | |
;                                 +---+---+

