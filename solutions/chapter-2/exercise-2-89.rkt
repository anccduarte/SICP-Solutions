
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.89.
;---
;Define procedures that implement the termlist representation described above as
;appropriate for dense polynomials.
;------------------------------------------------------------------------------------------

;Dense polynomials are simply represented as a list of coefficients. The orders may be
;omitted since, for a given term 't', its order is readily computed by calculating the
;length of the term sublist starting in 't' and subtracting one unit from it. If we wish
;to represent polynomials this way, we must simply alter a single selector for terms,
;namely 'first-term', and 'adjoin-term'. For 'first-term', instead of simply returning
;the term (as in the alternative representation), we compute the order 'ord' associated
;to the coefficient 'coeff' and return a list '(ord coeff). For 'adjoin-term', the
;process is simplified: instead of adjoining a list '(ord coeff), we simply adjoin the
;newly computed coefficient.
;---
(define (install-polynomial-package)
  ;internal procedures
  ;representation of polynomials [note that it remains unaltered]
  (define (make-poly var terms) (cons var terms))
  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))
  ;representation of terms [note that only the selector 'first-term' is altered; 'order'
  ;and 'coeff' work properly, since whenever we select a specific term from the list of
  ;terms (by calling 'first-term'), we "change" its representation to '(ord coeff); note
  ;that the code breaks if we select a term from the list of terms with 'car', since we
  ;would simply be extracting the coefficient from the term]
  ;...
  (define (first-term terms) (cons (- (length terms) 1) (car terms)))
  (define (rest-terms terms) (cdr terms))
  (define (order term) (car term))
  (define (coeff term) (cdr term))
  ;---
  (define (adjoin-term term term-list)
    (define (close-to-zero? coeff)
      (if (not (number? coeff))
          #f
          (< coeff 0.00001)))
    (let ((c (coeff term)))
      (if (close-to-zero? c)
          term-list
          (cons c term-list))))
  ;interface to rest of the system
  ;...
  (define (tag p) (attach-tag 'polynomial p))
  (put 'make 'polynomial (lambda (p) (tag (make-poly p)))))

