
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.16.
;---
;Design a procedure that evolves an iterative exponentiation process that uses successive
;squaring and uses a logarithmic number of steps, as does 'fast-expt'. (Hint: Using the
;observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the exponent 'n' and the
;base 'b', an additional state variable 'a', and define the state transformation in such
;a way that the product a*b^n is unchanged from state to state. At the beginning of the
;process 'a' is taken to be 1, and the answer is given by the value of 'a' at the end of
;the process. In general, the technique of defining an invariant quantity that remains
;unchanged from state to state is a powerful way to think about the design of iterative
;algorithms.)
;------------------------------------------------------------------------------------------

;'fast-exp-rec' -> generates a recursive process
;b^e = 1 if e == 0 else square(b^(e/2)) if even(e) else b*(b^(e-1))
;---
(define square (lambda (x) (* x x)))
(define even? (lambda (x) (= (remainder x 2) 0)))
(define (fast-exp-rec b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp-rec b (/ n 2))))
        (else (* b (fast-exp-rec b (dec n))))))

;'fast-exp-iter' -> generates an iterative process
;keep a third state variable 'a' so that a*b^n remains unchanged between calls
;if a is even => a*b^n = a*(b^(n/2))^2 = (a)*(b^2)^(n/2)
;if a is odd => a*b^n = a*b*b^(n-1) = (a*b)*(b)^(n-1)
;---
(define (fast-exp-iter b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (dec n)))))
  (iter 1 b n))

;test both implementations for random bases and exponents
;---
(define (test b n)
  (let ((x (fast-exp-rec b n))
        (y (fast-exp-iter b n)))
    (display (= x y)) (display " ")
    (display "(") (display b) (display "^") (display n) (display ") ")
    (display x) (display " ") (display y) (newline)))
;---
(test 2 4)
(test 3 3)
(test 2 19)

