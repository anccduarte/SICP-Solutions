
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.90.
;---
;Suppose we want to have a polynomial system that is efficient for both sparse and dense
;polynomials. One way to do this is to allow both kinds of term-list representations in
;our system. The situation is analogous to the complex-number example of Section 2.4,
;where we allowed both rectangular and polar representations. To do this we must
;distinguish different types of term lists and make the operations on term lists generic.
;Redesign the polynomial system to implement this generalization. THis is a major effort,
;not a local change.
;------------------------------------------------------------------------------------------

;Similarly to complex numbers, polynomials allow for more than one representation. Here,
;polynomials are proposed to be represented as dense and sparse. So that both
;representations may coexist in the same environment, a "vertical" abstraction barrier
;between them must be erected. Then, interface procedures permitting the communication
;with the rest of the system must be devised. Finally, a "generic" polynomial package
;ought to be implemented.

;dealing with type tags -> helper procedures
;---
(define (attach-tag tag data) (cons tag data))
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

;'apply-generic' -> helper [does not consider coercion]
;---
(define (apply-generic op . args)
  (let* ((type-args (map type-tag args))
         (proc (get op type-args)))
    (if proc
        (apply proc (map contents args))
        (error "no operation for data types"))))

;installing both representations of terms [dense and sparse]
;---
(define (install-dense-terms-package)
  ;internal procedures
  (define (first-term terms)
    (cons (- (length terms) 1) (car terms)))
  ;---
  (define (adjoin-term term term-list)
    (define (close-to-zero? x)
      (if (not (number? x))
          #f
          (< x 0.00001)))
    (if (close-to-zero? (coeff term))
        term-list
        (cons (coeff term) term-list)))
  ;interface to rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put 'make-termlist 'dense (lambda (t) (tag t)))
  (put 'empty-termlist? '(dense) (lambda (t) (null? t)))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense) (lambda (t) (tag (cdr t))))
  (put 'adjoin-term 'dense
       (lambda (t lst) (tag (adjoin-term t lst)))))
;---
(define (install-sparse-terms-package)
  ;internal procedures
  (define (adjoin-term term term-list)
    (define (close-to-zero? x)
      (if (not (number? x))
          #f
          (< x 0.00001)))
    (if (close-to-zero? (coeff term))
        term-list
        (cons term term-list)))
  ;interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'make-termlist 'sparse (lambda (t) (tag t)))
  (put 'empty-termlist? '(sparse) (lambda (t) (null? t)))
  (put 'first-term '(sparse) (lambda (t) (car t)))
  (put 'rest-terms '(sparse) (lambda (t) (tag (cdr t))))
  (put 'adjoin-term 'sparse
       (lambda (t lst) (tag (adjoin-term t lst)))))

;interface procedures [note that 'make-term', 'order' and 'coeff' are independent of data
;types: once stripped off the type of terms ('dense' or 'sparse'), each individual term
;becomes "exposed"; moreover, note the oddity of 'adjoin-term' - the procedure has to be
;implemented this way since individual terms are untyped: calling 'apply-generic' with
;arguments 'term' and 'term-list' would raise an error]
;---
(define (make-term ord coeff) (cons ord coeff))
(define (order term) (car term))
(define (coeff term) (cdr term))
;---
(define (make-dense-termlist) (get 'make-termlist 'dense))
(define (make-sparse-termlist) (get 'make-termlist 'sparse))
;---
(define (empty-termlist? terms)
  (apply-generic 'empty-termlist terms))
;---
(define (first-term terms)
  (apply-generic 'first-term terms))
;---
(define (rest-terms terms)
  (apply-generic 'rest-terms terms))
;---
(define (adjoin-term term term-list)
  (let* ((type-terms (type-tag term-list))
         (adjoin-proc (get 'adjoin-term type-terms)))
    (if adjoin-proc
        (adjoin-proc term term-list)
        (error "no adjoin procedure for type"))))

;constructing a "generic" polynomial package [note that operations on polynomials not of
;the same type are not supported: no error is raised, but the results won't match the
;expected in most cases]
;---
(define (install-polynomial-package)
  ;import procedures from dense and sparse packages
  ;internal procedures
  ;representation of polynomials
  (define (make-poly var terms) (cons var terms))
  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))
  ;operations on polynomials
  ;...
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
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "polynomials not of same variable")))
  ;interface to the rest of the system
  ;...
  (define (tag p) (attach-tag 'polynomial p))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2)))))

