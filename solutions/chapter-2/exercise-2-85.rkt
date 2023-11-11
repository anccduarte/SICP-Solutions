
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.85.
;---
;This section mentioned a method for "simplifying" a data object by lowering it in the
;tower of types as far as possible. Design a procedure 'drop' that accomplishes this for
;the tower described in Exercise 2.83. The key is to decide, in some general way, whether
;an object can be lowered. For example, the complex number (1.5 + 0i) can be lowered as
;far as real, the complex number (1 + 0i) can be lowered as far as integer, and the
;complex number (2 + 3i) cannot be lowered at all. Here is a plan for determining whether
;an object can be lowered: Begin by defining a generic operation project that "pushes" an
;object down in the tower. For example, projecting a complex number would involve
;throwing away the imaginary part. Then a number can be dropped if, when we 'project' it
;and 'raise' the result back to the type we started with, we end up with something equal
;to what we started with. Show how to implement this idea in detail, by writing a 'drop'
;procedure that drops an object as far as possible. You will need to design the various
;projection operations and install 'project' as a generic operation in the system. You
;will also need to make use of a generic equality predicate, such as described in
;Exercise 2.79. Finally, use 'drop' to rewrite 'apply-generic' from Exercise 2.84 so that
;it "simplifies" its answers.
;------------------------------------------------------------------------------------------

;'type-tag' and 'contents' -> helper procedures
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

;'raise' (from Exercise 2.83.)
;---
(define tower-of-types '(integer rational real complex))
;---
(define (memq elem lst)
  (cond ((null? lst)
         #f)
        ((eq? elem (car lst))
         lst)
        (else
         (memq elem (cdr lst)))))
;---
(define (raise arg)
  (let ((subtower (memq (type-tag arg) tower-of-types)))
    (if subtower
        (if (null? (cdr subtower))
            arg
            (let ((raiser (get-coercion (car subtower)
                                        (cadr subtower))))
              (if raiser
                  (raiser (contents arg))
                  (error "no coercion procedure for type"))))
        (error "type not in tower of types"))))

;designing the projection operations
;[assumes the existence of a table of projection operations]
;---
(define (install-rational-package)
  ;...
  (define (project-rational r)
    (make-integer (round (/ (numer r) (denom r)))))
  (put-project 'rational project-rational))
;---
(define (install-real-package)
  ;...
  (define (project-real r)
    ;['numerator' and 'denominator' are Scheme built-in procedures
    ;taking a real number as input, and finding the lowest integers
    ;such that its input is transformed in a rational number of
    ;equivalent value; e.g., the real 2.5 equates to the rational
    ;5/2 => (numerator 2.5) -> 5 and (denominator 2.5) -> 2]
    ;['inexact->exact' gets rid of a real number's decimal part if
    ;it is not relevant; i.e., equal to .000000...]
    (make-rational (inexact->exact (numerator r))
                   (inexact->exact (denominator r))))
  (put-project 'real project-real))
;---
(define (install-complex-package)
  ;...
  (define (project-complex c)
    (make-real (real-part c)))
  (put-project 'complex project-complex))

;installing the generic procedure 'project'
;---
(define (project arg)
  (get-project (type-tag arg)))

;'drop' (successive projection of its argument)
;[assumes the existence of a table of projection operations]
;---
(define (drop arg)
  (let ((proc-proj (project arg)))
    (if (not proc-proj)
        arg
        (let* ((proj-arg (proc-proj arg))
               (coerced-proj (raise proj-arg)))
          (if (equ? arg coerced-proj)
              (drop proj-arg)
              arg)))))

;For reasons of simplicity, 'apply-generic' is not redefined here. The update is quite
;straightforward: simply wrap the first and second calls of 'apply' (see inner procedure
;definitions of 'apply-generic' in Exercise 2.84.) in 'apply-generic' with the newly
;defined procedure 'drop'. 

