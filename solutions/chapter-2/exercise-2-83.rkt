
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.83.
;---
;Suppose you are designing a generic arithmetic system for dealing with the tower of
;types shown in Figure 2.25: 'integer', 'rational', 'real', 'complex' (SEE BELOW *). For
;each type (except 'complex'), design a procedure that raises objects of that type one
;level in the tower. Show how to install a generic raise operation that will work for
;each type (except 'complex').
;------------------------------------------------------------------------------------------

;(*) tower of types
;---
; complex numbers
;       ∧
;  real numbers
;       ∧
;   rationals
;       ∧
;   integers

;Helper procedures
;---
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad datum")))
;---
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad datum")))

;It is assumed that the procedures are already available (i.e., implementations of
;'integer->rational', 'rational->real' and 'real->complex' are provided). Moreover, it is
;assumed that each type may only be raised to the immediate type in the tower, that is,
;for a given type 't', we may assume that there is only one way to raise it to the next
;type, and that coercion on 't' is well defined. That is to say that integers are always
;coerced to rationals, rationals to real numbers, and real numbers to complex numbers.
;---
(define (raise arg)
  (define (get-target-type type-arg)
    (cond ((eq? type-arg 'integer) 'rational)
          ((eq? type-arg 'rational) 'real)
          ((eq? type-arg 'real) 'complex)
          (else (error "bad type"))))
  (define (raise-main)
    (let* ((arg-type (type-tag arg))
           (target-type (get-target-type arg-type))
           (proc-coercion (get arg-type target-type)))
      (proc-coercion arg)))
  (raise-main))

;Alternative implementation (the tower of types is explicitly defined)
;---
(define (tower-of-types '(integer rational real complex)))
;---
(define (raise-2 arg)
  (define (raise-iter tower)
    (if (null? types)
        (error "type of arg not found in the tower of types")
        (let ((type-arg (type-tag arg)))
          (if (eq? type-arg (car tower))
              (if (null? (cdr tower))
                  arg
                  (let ((raiser (get-coercion type-arg
                                             (cadr tower))))
                    (if raiser
                        (raiser (contents arg))
                        (error "no coercion procedure found for arg"))))
              (raise-iter (cdr tower))))))
  (raise-iter tower-of-types))

;Having implemented a generic 'raise' procedure, in order to be put into operation, an
;additional effort is needed: upon installation of every type (i.e., 'integer, 'rational,
;'real and 'complex), the respective coercion procedure must be installed. That is:
;---
(define (install-integer-package)
  ;...
  (define (integer->rational i) (make-rational i 1))
  (put-coercion 'integer 'rational integer->rational))
;---
(define (install-rational-package)
  ;...
  (define (rational->real r) (make-real (/ (numer r) (denom r))))
  (put-coercion 'rational 'real rational->real))
;---
(define (install-complex-package)
  ;...
  (define (real->complex r) (make-complex-from-rectangular r 0))
  (put-coercion 'real 'complex real->complex))

