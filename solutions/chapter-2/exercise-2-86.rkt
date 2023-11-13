
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.86.
;---
;Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes,
;and angles can be either ordinary numbers, rational numbers, or other numbers we might
;wish to add to the system. Describe and implement the changes to the system needed to
;accommodate this. You will have to define operations such as 'sine' and 'cosine' that
;are generic over ordinary numbers and rational numbers.
;------------------------------------------------------------------------------------------

;Helper procedures ['attach-tag', 'type-tag' and 'contents']
;---
(define (attach-tag tag data)
  (cons tag data))
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

;Definition of generic operators based on a data-directed approach [it is assumed that a
;a table indexed by operations and types is available]
;---
(define (ADD x y)
  (get 'add (map type-tag (list x y))))
;---
(define (SUB x y)
  (get 'sub (map type-tag (list x y))))
;---
(define (MUL x y)
  (get 'mul (map type-tag (list x y))))
;---
(define (DIV x y)
  (get 'div (map type-tag (list x y))))

;Generic selectors for complex numbers [it is assumed that the 'rectangular' and 'polar'
;packages are already installed; also, assume the implementation of 'apply-generic' as in
;Exercise 2.84. - with slightly distinct tower of types]
;---
(define (real-part z)
  (apply-generic 'real-part z))
;---
(define (imag-part z)
  (apply-generic 'imag-part z))
;---
(define (magnitude z)
  (apply-generic 'magnitude z))
;---
(define (angle z)
  (apply-generic 'angle z))

;Now, to accommodate the proposed changes, we must simply replace the Scheme built-in
;mathematical operations (e.g., '+', '-', '*' and "/") by the respective generic operator
;(i.e., 'ADD', 'SUB', 'MUL', 'DIV', respectively) wherever they occur in the installation
;of type-specific procedures. Below, the modifications made to 'install-complex-package'
;are illustrated. [Again, it is assumed that the 'rectangular' and 'polar' packages are
;already installed] By utilizing the generic operators instead of the built-in analogues,
;the system is instructed on how to behave depending on the types of numbers that compose
;any complex number.
;---
(define (install-complex-package)
  ;import procedures from 'rectangular' and 'polar' packages
  (define (make-rectangular x y)
    (get 'make-rectangular 'rectangular))
  (define (make-polar r a)
    (get 'make-polar 'polar))
  ;define internal procedures [to be installed]
  (define (add-complex z1 z2)
    (make-rectangular (ADD (real-part z1) (real-part z2))
                      (ADD (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-rectangular (SUB (real-part z1) (real-part z2))
                      (SUB (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-polar (MUL (magnitude z1) (magnitude z2))
                (ADD (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-polar (DIV (real-part z1) (real-part z2))
                (SUB (angle z1) (angle z2))))
  ;install procedures in the table [interface to the system]
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (dic-complex z1 z2))))
  (put 'make-rectangular-complex 'complex
       (lambda (x y) (tag (make-rectangular x y))))
  (put 'make-polar-complex 'complex
       (lambda (r a) (tag (make-polar r a)))))

;Operations such as 'sine' and 'cosine' must only be defined for Scheme numbers and
;rationals (defining these operations for complex numbers in nonsensical)
;---
(define (install-scheme-number-package)
  ;internal procedures
  ;...
  ;interface to the rest of the system
  ;...
  (put 'sine 'scheme-number sin)
  (put 'cosine 'scheme-number cos))
;---
(define (install-rational-package)
  ;internal procedures
  ;...
  (define (sine r) (sin (/ (num r) (denom r))))
  (define (cosine r) (cos (/ (num r) (denom r))))
  ;interface to the rest of the system
  ;...
  (put 'sine 'rational sine)
  (put 'cosine 'rational cosine))

;Installing these operations immediately allows for the implementation and use of generic
;operations pertaining the computation of the sine and cosine of scheme numbers and
;rational numbers [again, assume the implementation of 'apply-generic' as in Exercise
;2.84. - with slightly distinct tower of types]
;---
(define (sine x)
  (apply-generic 'sine x))
;---
(define (cosine x)
  (apply-generic 'cosine x))

