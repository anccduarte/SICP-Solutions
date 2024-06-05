
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.15.
;---
;Given a one-argument procedure 'p' and an object 'a', 'p' is said to "halt" on 'a' if
;evaluating the expression (p a) returns a value (as opposed to terminating with an error
;message or running forever). Show that it is impossible to write a procedure 'halts?'
;that correctly determines whether 'p' halts on 'a' for any procedure 'p' and object 'a'.
;Use the following reasoning: If you had such a procedure 'halts?', you could implement
;the following program:
;---
;(define (run-forever) (run-forever))
;(define (try p)
;  (if (halts? p p) (run-forever) 'halted))
;---
;Now consider evaluating the expression (try try) and show that any possible outcome
;(either halting or running forever) violates the intended behavior of 'halts?'.
;------------------------------------------------------------------------------------------

;Proof by contradiction
;[heavily inspired by Sipser's proof in "Introduction to the Theory of Computation"]
;---
;Suppose we have a procedure 'H' that determines whether a procedure 'p', when applied to
;its sole argument <a>, halts and accepts <a> or halts and rejects its argument. In
;essence, 'H' is a procedure such that
;---
;H(<p,a>) = | accept if 'p' accepts <a>
;           | reject if 'p' does not accept <a>
;---
;Assuming that 'H' does exist, we may readily construct procedure 'D' of one argument 'p'
;that extends the functioning of 'H' as follows: 1. executes 'H' with arguments <p, <p>>
;(i.e., checks the halting behavior of 'p' run on <p>); 2. outputs the opposite of the
;outcome of 'H'. In other words, 'D' is a procedure such that
;---
;D(<p>) = | accept if 'p' does not accept <p>
;         | reject if 'p' accepts <p>
;---
;Now, suppose that we run 'D' having <D> has its input. In such a circumstance, we get
;the following behavior
;---
;D(<p>) = | accept if 'D' does not accept <D>
;         | reject if 'D' accepts <D>
;---
;"No matter what 'D' does, it is forced to do the opposite, which obviously constitutes a
;contradiction. Thus, neither 'D' nor 'H' can exist."

