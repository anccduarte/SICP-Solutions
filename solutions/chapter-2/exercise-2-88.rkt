
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.88.
;---
;Extend the polynomial system to include subtraction of polynomials. (Hint: You may find
;it helpful to define a generic negation operation.)
;------------------------------------------------------------------------------------------

;generic 'negate'
;---
(define (negate arg)
  (apply-generic 'negate arg))

;installing negation operators for each data type [note that 'sub-terms' is implemented
;at 'install-polynomial-package']
;---
(define (install-scheme-number-package)
  ;internal procedures
  ;...
  ;interface to rest of the system
  ;...
  (put 'negate 'scheme-number (lambda (x) (- x))))
;---
(define (install-rational-package)
  ;internal procedures
  ;...
  (define (make-rational n d) (cons n d))
  (define (num r) (car r))
  (define (denom r) (cdr r))
  ;interface to rest of the system
  ;...
  (define (tag p) (attach-tag 'rational p))
  (put 'negate 'rational
       (lambda (r) (tag (make-rational (negate (num r))
                                       (denom r))))))
;---
(define (install-complex-package)
  ;import procedures from 'rectangular' and 'polar'
  (define make-rectangular (get 'make-rectangular 'rectangular))
  (define make-polar (get 'make-polar 'polar))
  ;internal procedures
  ;...
  ;interface to rest of the system
  ;...
  (define (tag p) (attach-tag 'complex p))
  (put 'negate 'complex
       (lambda (z) (tag (make-rectangular (negate (real-part z))
                                          (negate (imag-part z)))))))
;---
(define (install-polynomial-package)
  ;internal procedures
  ;representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))
  ;representation of terms and term lists
  ;...
  (define (adjoin-term term term-list)
    (define (close-to-zero? coeff)
      (if (not (number? coeff))
          #f
          (< coeff 0.00001)))
    (if (close-to-zero? (coeff term))
        term-list
        (cons term term-list)))
  ;---
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term terms) (car terms))
  (define (rest-terms terms) (cdr terms))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;---
  (define (negate-terms term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (let ((term (first-term term-list))
              (rest (rest-terms term-list)))
          (cons (make-term (order term)
                           (negate (coeff term)))
                (negate-terms rest)))))
  ;---
  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))
  ;---
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1
                                 (add-terms (rest-terms L1) L2)))
                   ((> (order t2) (order t1))
                    (adjoin-term t2
                                 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1)
                                            (add (coeff t1) (coeff t2)))
                                 (add-terms (rest-terms L1)
                                            (rest-terms L2)))))))))
  ;---
  (define (add-poly p1 p2)
    (if (eq? (variable p1) (variable p2))
        (make-poly (variable p1) (add-terms (term-list p1)
                                            (term-list p2)))
        (error "polynomials not of same variable")))
  ;---
  (define (sub-poly p1 p2)
    (add-poly p1 (negate p2)))
  ;interface to rest of the system
  ;...
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'negate 'polynomial
       (lambda (p) (tag (negate-poly p)))))

