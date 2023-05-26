
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.19.
;---
;Consider the change-counting program of Section 1.2.2. It would be nice to be able to
;easily change the currency used by the program, so that we could compute the number of
;ways to change a British pound, for example. As the program is written, the knowledge of
;the currency is distributed partly into the procedure 'first-denomination' and partly
;into the procedure 'count-change' (which knows that there are five kinds of U.S. coins).
;It would be nicer to be able to supply a list of coins to be used for making change. We
;want to rewrite the procedure cc so that its second argument is a list of the values of
;the coins to use rather than an integer specifying which coins to use. We could then
;have lists that defined each kind of currency:
;(define us-coins (list 50 25 10 5 1))
;(define uk-coins (list 100 50 20 10 5 2 1 0.5))
;We could then call cc as follows:
;(cc 100 us-coins) -> 292
;To do this will require changing the program 'cc' somewhat. It will still have the same
;form, but it will access its second argument differently, as follows (SEE BELOW *).
;Define the procedures 'first-denomination', 'except-first-denomination', and 'no-more?'
;in terms of primitive operations on list structures. Does the order of the list
;'coin-values' affect the answer produced by 'cc'? Why or why not?
;------------------------------------------------------------------------------------------

;'no-more?'
;---
(define (no-more? lst) (null? lst))

;'except-first-denomination'
;---
(define (except-first-denomination lst) (cdr lst))

;'first-denomination'
;---
(define (first-denomination lst) (car lst))

;(*) new 'cc' procedure
;---
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;test for random amount and for two kinds of currency
;---
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
;---
(display "us-coins (amount=100) -> ") (cc 100 us-coins)
(display "uk-coins (amount=100) -> ") (cc 100 uk-coins)

;answer to last part of the exercise
;---
;all combinations of coins are evaluated no matter the order by which each coin appears
;in the list. by changing the order of the coins, we only modify the order by which each
;combination is evaluated. hence, the order of the coins in the list does not affect the
;answer produced by 'cc'

