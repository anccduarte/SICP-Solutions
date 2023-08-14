
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.80.
;---
;Define a generic predicate '=zero?' that tests if its argument is zero, and install it
;in the generic arithmetic package. This operation should work for ordinary numbers,
;rational numbers, and complex numbers.
;------------------------------------------------------------------------------------------

;'scheme-number' package
;---
(define (install-scheme-number-package)
  ;no internal definitions
  ;interface to the rest of the system
  ;...
  (put '=zero?
       '(scheme-number)
       (lambda (x) (= x 0)))
  'DONE)
;---
(install-scheme-number-package)

;'rational-number' package
;---
(define (install-rational-package)
  ;internal procedure definitions
  ;...
  ;interface to the rest of the system
  ;...
  (put '=zero?
       '(rational-number)
       (lambda (x) (and (= (numer x) 0)
                        (not (= (denom x) 0)))))
  ;...
  'DONE)
;---
(install-rational-package)

;'complex-number' package
;---
(define (install-complex-package)
  ;imported procedures from rectangular and polar packages
  ;...
  ;internal procedure definitions
  ;...
  ;interface to the rest of the system
  ;...
  (put '=zero?
       '(complex-number)
       (lambda (z) (= (magnitude z) 0)))
  ;...
  'DONE)

;defining generic operators
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
(define (=zero? x)
  (apply-generic 'zero? x))

