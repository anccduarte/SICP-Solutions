
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.91.
;---
;A univariate polynomial can be divided by another one to produce a polynomial quotient
;and a polynomial remainder. For example,
;---
;(x^5 - 1) / (x^2 - 1) = (x^3 + x), remainder (x - 1)
;---
;Division can be performed via long division. That is, divide the highest-order term of
;the dividend by the highest-order term of the divisor. The result is the first term of
;the quotient. Next, multiply the result by the divisor, subtract that from the dividend,
;and produce the rest of the answer by recursively dividing the difference by the
;divisor. Stop when the order of the divisor exceeds the order of the dividend and
;declare the dividend to be the remainder. Also, if the dividend ever becomes zero,
;return zero as both quotient and remainder.
;---
;We can design a 'div-poly' procedure on the model of 'add-poly' and 'mul-poly'. The
;procedure checks to see if the two polys have the same variable. If so, 'div-poly'
;strips off the variable and passes the problem to 'div-terms', which performs the
;division operation on term lists. 'div-poly' finally reattaches the variable to the
;result supplied by 'div-terms'. It is convenient to design 'div-terms' to compute both
;the quotient and the remainder of a division. 'div-terms' can take two term lists as
;arguments and return a list of the quotient term list and the remainder term list.
;---
;Complete the following definition of 'div-terms' by filling in the missing expressions.
;Use this to implement 'div-poly', which takes two polys as arguments and returns a list
;of the quotient and remainder polys.
;---
;(define (div-terms L1 L2)
;  (if (empty-termlist? L1)
;      (list (the-empty-termlist) (the-empty-termlist))
;      (let ((t1 (first-term L1))
;            (t2 (first-term L2)))
;        (if (> (order t2) (order t1))
;            (list (the-empty-termlist) L1)
;            (let ((new-c (div (coeff t1) (coeff t2)))
;                  (new-o (- (order t1) (order t2))))
;              (let ((rest-of-result
;                     <compute rest of result recursively>))
;                <form complete result>))))))
;------------------------------------------------------------------------------------------

;'div-terms' [completed]
;[<compute rest of result recursively> is thoroughly explained above; hence, there is no
;need for more details]
;[<form complete result>'s logic is more intricate and unveils the purpose of returning a
;list of two empty term lists whenever the dividend is empty and a list of an empty term
;list and the dividend itself whenever the order of the divisor is larger than that of
;the dividend. Whenever 'rest-of-result' falls under any of these cases, a pair of term
;lists is returned, and 'res-term' is adjoined to the first. At this moment, all
;recursive calls to 'div-terms' are sequentially wrapped up by following the exact same
;proceeding as before, that is, the 'res-term's defined in the respective environments
;(from lower to higher orders) are adjoined to the first list of the previous result. The
;second term list returned by 'div-terms' persists from its original inception, that is,
;it is determined as 'div-terms' collapses to a base case (i.e., either the dividend is
;empty or the order of the divisor is larger than that of the dividend).]
;---
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((res-term (make-term (- (order t1) (order t2))
                                       (div (coeff t1) (coeff t2)))))
              (let ((rest-of-result
                     (div-terms (sub-terms L1
                                           (mul-term-by-all-terms res-term
                                                                  L2))
                                L2)))
                (list (adjoin-term res-term (car rest-of-result))
                      (cadr rest-of-result))))))))

;'div-poly' [new]
;[Since 'div-terms' essentially returns a pair of term lists, 'div-poly' should also
;return a pair o polynomials: one corresponding to the quotient and the other pertaining
;to the remainder.]
;---
(define (div-poly p1 p2)
  (if (eq? (variable p1) (variable p2))
      (let ((result (div-terms (term-list p1)
                               (term-list p2))))
        (list (make-poly (variable p1) (car result))
              (make-poly (variable p1) (cadr result))))
      (error "polynomials not of same variable")))

