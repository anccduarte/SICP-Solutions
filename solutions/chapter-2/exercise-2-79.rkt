
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.79.
;---
;Define a generic equality predicate 'equ?' that tests the equality of two numbers, and
;install it in the generic arithmetic package. This operation should work for ordinary
;numbers, rational numbers, and complex numbers.
;------------------------------------------------------------------------------------------

;'scheme-number' package
;(note that the internal type system of scheme is used here)
;---
(define (install-scheme-number-package)
  ;no internal definitions
  ;interface to the rest of the system
  ;...
  (put 'equ? '(scheme-number scheme-number) =)
  'DONE)
;---
(install-scheme-number-package)

;'rational-number' package
;(note that in the equality test it is assumed that 'make-rat'
;produces rational in reduced form; moreover, any time a
;denominator has sign '-', both the numerator and denominator
;invert their signs [see below])
;---
(define (install-rational-package)
  ;internal procedure definitions
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (< d 0)
        (make-rat (- n) (- d))
        (let ((g (gcd (abs n) d)))
          (cons (/ n g) (/ d g)))))
  ;...
  ;interface to the rest of the system
  (define (tag x) (attach-tag 'rational-number x))
  ;...
  (put 'equ?
       '(rational-number rational-number)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put 'make
       'rational-number
       (lambda (n d) (tag (make-rat n d))))
  'DONE)
;---
(install-rational-package)

;'complex-number' package
;(note that selectors for the real and imaginary parts of the
;complex numbers are utilized; selectors for the magnitude and
;angle may also be used)
;(also note that a procedure 'close2zero?' is used to verify
;whether the real and imaginary parts of two complex numbers
;are equal; this is useful whenever we want to check for the
;equality of two complex numbers built under distinct data
;representations - rectangular or polar)
;---
(define (install-complex-package)
  ;import procedures from rectangular and polar packages
  (define (make-rectangular x y)
    ((get 'make-rectangular 'rectagular) x y))
  (define (make-polar r a)
    ((get 'make-polar 'polar) r a))
  ;internal procedure definitions
  (define (close2zero? x)
    (let ((tolerance 0.0000001))
      (< (abs x) tolerance)))
  ;...
  ;interface to the rest of the system
  (define (tag x) (attach-tag 'complex-number x))
  ;...
  (put 'equ?
       '(complex-number complex-number)
       (lambda (x y)
         (and (close2zero? (- (real-part x) (real-part y)))
              (close2zero? (- (imag-part x) (imag-part y))))))
  (put 'make-rectangular
       'complex
       (lambda (x y) (tag (make-rectagular x y))))
  (put 'make-polar
       'complex
       (lambda (r a) (tag (make-polar r a))))
  'DONE)
;---
(install-complex-package)

;defining generic procedures
;---
(define (apply-generic op . args)
  (let ((type-args (map type-tag args)))
    (let ((proc (get op type-args)))
      (if proc
          (apply proc (map contents args))
          (error "bad operation for data types:"
                 (list op type-args))))))
;---
;...
(define (equ? x y)
  (apply-generic 'equ? x y))

