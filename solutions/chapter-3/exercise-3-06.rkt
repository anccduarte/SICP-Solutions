
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.6.
;---
;It is useful to be able to reset a random-number generator to produce a sequence
;starting from a given value. Design a new 'rand' procedure that is called with an
;argument that is either the symbol 'generate' or the symbol 'reset' and behaves as
;follows: (rand 'generate) produces a new random number; ((rand 'reset) <new-value>)
;resets the internal state variable to the designated <new-value>. Thus, by resetting the
;state, one can generate repeatable sequences. These are very handy to have when testing
;and debugging programs that use random numbers.
;------------------------------------------------------------------------------------------

;'random-init' is arbitrarily initialized to 100
;---
(define random-init 100)

;'rand-update'
;(simply adds one unit to its argument - the decision on the mechanics of the procedure
;is arbitrary, as it solely serves the purpose of operationalizing 'rand')
;---
(define (rand-update x) (+ x 1))

;'rand'
;---
(define rand
  (let ((x random-init))
    (lambda (action)
      (cond ((eq? action 'generate)
             (set! x (rand-update x)) x)
            ((eq? action 'reset)
             (lambda (new-value) (set! x new-value)))
            (else
             (display "Unknown action!") (newline))))))

;test 'rand'
;---
(rand 'other-action)
(rand 'generate)
(rand 'generate)
((rand 'reset) 100)
(rand 'generate)

