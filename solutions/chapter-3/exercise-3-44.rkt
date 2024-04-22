
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.44.
;---
;Consider the problem of transferring an amount from one account to another. Ben
;Bitdiddle claims that this can be accomplished with the following procedure, even if
;there are multiple people concurrently transferring money among multiple accounts, using
;any account mechanism that serializes deposit and withdrawal transactions, for example,
;the version of 'make-account' in the text above.
;---
;(define (transfer from-account to-account amount)
;  ((from-account 'withdraw) amount)
;  ((to-account 'deposit) amount))
;---
;Louis Reasoner claims that there is a problem here, and that we need to use a more
;sophisticated method, such as the one required for dealing with the exchange problem. Is
;Louis right? If not, what is the essential difference between the transfer problem and
;the exchange problem? (You should assume that the balance in 'from-account' is at least
;'amount'.)
;------------------------------------------------------------------------------------------

;Is Louis right? If not, what is the essential difference between the transfer problem
;and the exchange problem?
;---
;Louis is wrong. The issue with 'exchange' lies in the fact that it explicitly computes
;the amount to be withdrawn/deposited from/to the accounts ('difference'). Such a
;circumstance is problematic if the interleaving of exchange operations occurs in such a
;way that these computed amounts are, at the end, miscalculated (see "exercise-3-43.rkt"
;for a concrete example). On the other hand, 'transfer' does not compute any amount to be
;transferred as it is predetermined by its argument 'amount'. This subtle yet decisive
;distinction does not leave any room for hypothetic errors; hence, no matter how many
;concurrent transfers are consummated, the program will always behave correctly.

