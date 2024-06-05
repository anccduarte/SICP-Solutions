
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
;D(<D>) = | accept if 'D' does not accept <D>
;         | reject if 'D' accepts <D>
;---
;"No matter what 'D' does, it is forced to do the opposite, which obviously constitutes a
;contradiction. Thus, neither 'D' nor 'H' can exist."

;Embodying 'H' and 'D'
;[note that two versions of 'D' are provided - see discussion in next section]
;---
(define (H p a) 'assume-existence)
;---
(define (D1 p)
  (if (eq? (H p p) 'accept)
      'reject
      'accept))
;---
(define (D2 p a)
  (if (eq? (H p a) 'accept)
      'reject
      'accept))

;Discussing the behavior of 'D'
;---
;As previously mentioned, 'D1' sets the logical bind by subverting the outcome of 'P'.
;Calling (D1 D1) yields 'accept if (D1 D1) results in 'reject and yields 'reject if the
;outcome of (D1 D1) is 'accept. This obviously constitutes a contradiction. The reason
;for calling 'D1' on 'D1' is a bit hermetic at first glance. Suppose that instead of
;calling 'D1' on 'D1' itself, we runned it on a putative procedure 'R'. Then, executing
;(D1 R) returns 'accept if 'R' rejects itself and 'reject if 'R' accepts itself. This
;does not constitute a contradiction, and thus the reason for the self-referential call.
;---
;The reason for materializing 'D2' is an attempt to answer the question "Why does 'D' has
;to be a one-argument procedure?". 'H' applies (somewhere in its body definition) 'P' to
;'A' (remember that 'P' must, in this case, be a one-argument procedure). Defining 'D' as
;a procedure of two arguments is ruinous, since it would "furtively" result in an error
;when running (D2 D2 <arg>). The error would be thrown at the time 'H' is executed with
;arguments 'D2' and <arg> since 'H' is to make the call (D2 <arg>) in its inner workings,
;resulting in a mismatch in the number of formal parameters and arguments of 'D2'. Hence,
;'D' must imperatively be a single-argument procedure.

