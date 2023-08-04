
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.73.
;---
;Section 2.3.2. described a program that performs symbolic differentiation (SEE BELOW *).
;We can regard this program as performing a dispatch on the type of the expression to be
;differentiated. In this situation the "type tag" of the datum is the algebraic operator
;symbol (such as "+") and the operation being performed is "deriv". We can transform this
;program into data-directed style by rewriting the basic derivative procedure as (SEE
;BELOW **).
;---
;(a) Explain what was done above. Why can't we assimilate the predicates "number?" and
;"variable?" into the data-directed dispatch?
;---
;(b) Write the procedures for derivatives of sums and products, and the auxiliary code
;required to install them in the table used by the program above (SEE BELOW **).
;---
;(c) Choose any additional differentiation rule that you like, such as the one for
;exponents (Exercise 2.56.), and install it in this data-directed system.
;---
;(d) In this simple algebraic manipulator the type of an expression is the algebraic
;operator that binds it together. Suppose, however, we indexed the procedures in the
;opposite way, so that the dispatch line in "deriv" looked like
;---
;((get (operator exp) 'deriv) (operands exp) var)
;---
;What corresponding changes to the derivative system are required?
;------------------------------------------------------------------------------------------

;(*) original program for symbolic differentiation
;---
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                   (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
        ;<more rules can be added here>
        (else (error "unknown expression type: DERIV" exp))))

;(**) data-directed programming approach for symbolic differentiation
;---
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
;---
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;(a)
;---
;A data-directed approach was used to encapsulate all derivative rules (except for the
;ones applied whenever the predicates "number?" or "variable?" yield #t) in a single
;conditional clause. The idea is pertinent since for the same mathematical process,
;namely derivation, there exist many rules depending on the operations present in the
;expression "deriv" is acting on (e.g., rules for addition of subexpressions,
;multiplication of subexpressions, etc.). More concretely, a table consisting of a single
;row labeled by the symbol 'deriv and with columns representing operations for which a
;derivative rule is well defined (e.g., addition: '+, multiplication: '*, etc.) is
;created. Sitting in the table are the procedures that abstract a derivative rule for a
;given mathematical operation:
;---
;         |    '+     |     '*    |    '**    | ...
; --------+-----------+-----------+-----------+-----
;  'deriv | deriv-sum | deriv-mul | deriv-exp | ...
;---
;The predicates "number?" and "variable?" cannot be assimilated into the data-directed
;dispatch because of the way the procedures "operator" and "operands" are implemented.
;Since numbers and variables are not represented as pairs/lists, applying these
;procedures to such data would result in an error. As noted in JoT's Jottings blog post
;(http://jots-jottings.blogspot.com/2011/12/sicp-exercise-273-data-directed.html), this
;issue may be circumvented by slightly modifying the selector procedures "operator" and
;"operands". New definitions of "operator" and "operands", and procedures for installing
;the rules for derivatives of numbers and variables are provided below:
;---
(define (operator exp)
  (cond ((number? exp) 'number)
        ((variable? exp) 'variable)
        (else (car exp))))
;---
(define (operands exp)
  (if (pair? exp) (cdr exp) exp))
;---
(define (install-number-deriv)
  ;defining the procedure
  (define (number-deriv exp var) 0)
  ;installing it in the system
  (put 'deriv 'number number-deriv))
;---
(define (install-variable-deriv)
  ;defining the procedure
  (define (variable-deriv exp var)
    (if (same-variable? exp var) 1 0))
  ;installing it in the system
  (put 'deriv 'variable variable-deriv))
;---
;Given the previous modifications, "deriv" may be implemented solely as a data-directed
;dispatch, simplifying the procedure to the point where a case analysis is no longer
;needed
;---
(define (deriv exp var)
  (let ((deriv-proc (get 'deriv (operator exp))))
    (deriv-proc (operands exp) var)))

;(b)
;---
(define (install-sum-deriv)
  ;(note that "addend" and "augend" are slightly distinct from
  ;the previous implementation - since the operator is stripped
  ;off in "deriv", the "addend" and the "augend" are simply the
  ;"car" and the "cadr" of the expression, respectively)
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))
  ;defining the derivative procedure
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  ;installing it in the system
  (put 'deriv '+ sum-deriv))
;---
(define (install-mul-deriv)
  ;(same logic as in "addend" and "augend" applies to the both
  ;procedures "multiplier" and "multiplicand")
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-mul m1 m2) (list '* m1 m2))
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cadr operands))
  ;defining the derivative procedure
  (define (mul-deriv operands var)
    (make-sum (make-mul (multiplier operands)
                        (deriv (multiplicand operands) var))
              (make-mul (deriv (multiplier operands) var)
                        (multiplicand operands))))
  ;installing it in the system
  (put 'deriv '* mul-deriv))

;(c)
;---
(define (install-exp-deriv)
  ;(same logic as in "addend" and "augend" applies to the both
  ;procedures "base" and "exponent"; also, it is assumed that the
  ;exponent of an expression is always a number)
  (define (make-mul m1 m2) (list '* m1 m2))
  (define (make-exp b e) (list '** b e))
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))
  ;defining the derivative procedure
  (define (exp-deriv operands var)
    (make-mul (exponent operands)
              (make-mul (deriv (base operands) var)
                        (make-exp (base operands)
                                  (- (exponent operands) 1)))))
  ;installing it in the system
  (put 'deriv '** exp-deriv))

;(d)
;---
;Indexing the procedures in the opposite way would not result in any significant change
;in the derivative system. The only modification to be made would have been to swap the
;order in which the types are registered when installing a derivative rule in the system.
;For example, instead of installing "sum-deriv" as (put 'deriv '+ sum-deriv), we would
;write (put '+ 'deriv sum-deriv)

