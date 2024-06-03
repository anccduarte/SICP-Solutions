
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

;Proof by counterexample
;---
;Suppose we have a procedure 'halts?' that determines whether a procedure 'p', when
;applied to its sole argument 'a', halts. This procedure returns #t if the execution of
;(p a) halts, and #f otherwise. So far, so good. Now, suppose we devise a procedure 'try'
;that, given a procedure 'p' as input, first tests (halts? p p) and proceeds as follows:
;if it does halt, it enters an infinite loop; else, it returns the symbol 'halted. Given
;these constraints, consider calling (try try). The program starts by performing the test
;(halts? try try). It then, counterintuitively, enters an infinite loop if the program
;halts and returns the symbol 'haulted otherwise. Note that the described modus operandi
;constitutes a counterexample in the sense that calling 'halts?' with both arguments 'p'
;and 'a' set to 'try' misjudges whether (try try) halts. Hence, it is impossible to
;compose a procedure 'halts?' that correctly determines whether 'p' halts on 'a' for any
;given procedure 'p' and object 'a'. [See Geoffrey K. Pullum's brilliant poem available
;at "http://www.lel.ed.ac.uk/~gpullum/loopsnoop.html" for "A proof that the Halting
;Problem is undecidable".]

