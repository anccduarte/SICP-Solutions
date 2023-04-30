
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.20.
;---
;The process that a procedure generates is of course dependent on the rules used by the
;interpreter. As an example, consider the iterative 'gcd' procedure given above (SEE
;BELOW *). Suppose we were to interpret this procedure using normal-order evaluation, as
;discussed in Section 1.1.5. (The normal-order evaluation rule for 'if' is described in
;Exercise 1.5.) Using the substitution method (for normal-order), illustrate the process
;generated in evaluating (gcd 206 40) and indicate the remainder operations that are
;actually performed. How many remainder operations are actually performed in the
;normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?
;------------------------------------------------------------------------------------------


;'gcd' by successive subtraction -> gcd(a,b) = gcd(max(t),min(t)), t=(b,a-b)
;---
;assume that d is the greatest common divisor of a and b. if d is a divisor of a and b,
;it must be that d is also a divisor of a and (a-b):
;if d is a divisor of a, then d=a/p for some p ∈ Z. so, a=d*p. similarly, if d is a
;divisor of b, then d=b/q for some q ∈ Z. so, b=d*q. therefore, we can rewrite (a-b) as
;(dp-dq) = d(p-q), which is divisible by d.

;'gcd' by successively computing remainders -> gcd(a,b) = gcd(b,a%b)
;---
;a%b -> remainder of the division of a by b
;a = nb + r => r = a - nb. like before, if d is a divisor of a and b, then a and b can be
;rewritten, respectively, as d*p and d*q for p,q ∈ Z.thus, r = a - nb can be rewritten as
;dp - ndq = d(p - nq), which is divisible by d. therefore, gcd(a,b) = gcd(b,r), r=a%b


;(*) compute the 'gcd' of two integers a and b
;---
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;------------------------------------------------------------------------------------------
;PART 1
;normal-order evaluation
;evaluate all operators first, then the operands
;------------------------------------------------------------------------------------------

(display "NORMAL-ORDER EVALUATION") (newline)
;---
(gcd 206 40)
;---
(gcd 40
     (remainder 206 40))
;b != 0 -> 1 evaluation
;---
(gcd (remainder 206 40)
     (remainder 40
                (remainder 206 40)))
;b != 0 -> 1 + 2 = 3 evaluations
;---
(gcd (remainder 40
                (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40))))
;b != 0 -> 3 + 4 = 7 evaluations
;---
(gcd (remainder (remainder 206 40)
                (remainder 40
                           (remainder 206 40)))
     (remainder (remainder 40
                           (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40
                                      (remainder 206 40)))))
;b = 0 -> 7 + 7 = 14 evaluations
;---
(remainder (remainder 206 40)
           (remainder 40
                      (remainder 206 40)))
;14 + 4 = 18 evaluations


;------------------------------------------------------------------------------------------
;PART 2
;applicative-order evaluation
;evaluate operands before applying the operator
;------------------------------------------------------------------------------------------

(newline) (display "APPLICATIVE-ORDER EVALUATION") (newline)
;---
(gcd 206 40)
(gcd 40 (remainder 206 40)) ;1 evaluation
(gcd 40 6)
(gcd 6 (remainder 40 6)) ;1 + 1 = 2 evaluations
(gcd 6 4)
(gcd 4 (remainder 6 4)) ;2 + 1 = 3 evaluations
(gcd 4 2)
(gcd 2 (remainder 4 2)) ;3 + 1 = 4 evaluations
(gcd 2 0)
2


;------------------------------------------------------------------------------------------
;CONCLUSION
;------------------------------------------------------------------------------------------

;normal-order evaluation -> 18 calls to 'remainder'
;applicative-order evaluation -> 4 calls to 'remainder'

