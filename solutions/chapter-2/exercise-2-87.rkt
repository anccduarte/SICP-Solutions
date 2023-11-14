
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.87.
;---
;Install '=zero?' for polynomials in the generic arithmetic package. This will allow
;'adjoin-term' to work for polynomials with coefficients that are themselves polynomials.
;------------------------------------------------------------------------------------------

;'install-polynomial-package' -> naive implementation
;---
(define (install-polynomial-package)
  ;internal procedures
  ;...
  (define (make-poly variable term-list)
    (cons variable term-list))
  ;interface to rest of the system
  ;...
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? 'polynomial (lambda (poly) (null? poly)))
  (put 'make 'polynomial (lambda (v t) (tag (make-poly v t)))))

;The previous implementation suggests that upon the generation of a polynomial, all terms
;whose coefficient is equal to zero are discarded. Yet, that is not the case (see
;definition of 'make-poly' in 'install-polynomial-package'). For example, constructing a
;polynomial by calling 'make-poly' with 'x and '((5 0) (3 0) (2 0) (0 0)) is valid and
;the resulting term list is not empty (although the polynomial is unequivocally equal to
;zero). Hence, to circumvent such a problem, one of two strategies may be adopted:
;---
;1. Modify 'make-poly' in 'install-polymial-package' to account for the fact that terms
;   of coefficient equal to zero may be provided to the constructor
;2. Implement a more robust version of '=zero?' that inspects all terms of the polynomial
;   it takes as input and checks whether all its coefficients are equal to zero

;1.
;---
(define (install-polynomial-package)
  ;internal procedures
  ;...
  (define (make-poly variable term-list)
    (define (close-to-zero? c)
      (if (not (number? c))
          #f
          (< c 0.00001)))
    (define (add-term term result)
      (if (close-to-zero? (coeff term))
          result
          (append result (list term))))
    (define (make-poly-iter terms result)
      (if (null? terms)
          (cons variable result)
          (make-poly-iter (rest-terms terms)
                          (add-term (first-term terms) result))))
    (make-poly-iter term-list '()))
  ;interface to rest of the system
  ;...
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? 'polynomial (lambda (poly) (null? poly)))
  (put 'make 'polynomial (lambda (v t) (tag (make-poly v t)))))

;2.
;---
(define (install-polynomial-package)
  ;internal procedures
  ;...
  (define (=zero? poly)
    (define (close-to-zero? c)
      (if (not (number? c))
          #f
          (< c 0.00001)))
    (define (zero-iter term-list)
      (if (empty-term-list? term-list)
          #t
          (let ((c (coeff (first-term term-list))))
            (if (not (close-to-zero? c))
                #f
                (zero-iter (rest-terms term-list))))))
    (zero-iter (term-list poly)))
  ;interface to rest of the system
  ;...
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? 'polynomial =zero?)
  (put 'make 'polynomial (lambda (v t) (cons v t))))

