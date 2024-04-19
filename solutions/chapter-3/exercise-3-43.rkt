
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.43.
;---
;Suppose that the balances in three accounts start out as $10, $20, and $30, and that
;multiple processes run, exchanging the balances in the accounts. Argue that if the
;processes are run sequentially, after any number of concurrent exchanges, the account
;balances should be $10, $20, and $30 in some order. Draw a timing diagram like the one
;in Figure 3.29 to show how this condition can be violated if the exchanges are
;implemented using the first version of the account-exchange program in this section. On
;the other hand, argue that even with this exchange program, the sum of the balances in
;the accounts will be preserved. Draw a timing diagram to show how even this condition
;would be violated if we did not serialize the transactions on individual accounts.
;------------------------------------------------------------------------------------------

;'exchange'
;---
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;'serialized-exchange'
;[assumes that 'make-account' returns a dispatcher that accepts 'serializer as message]
;---
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;Argue that if the processes are run sequentially, after any number of concurrent
;exchanges, the account balances should be maintained in some order.
;---
;'serialized-exchange' avoids concurrent mutation of an account's balance if this account
;participates in more than one exchange at a time. Assuming the accounts a1, a2 and a3,
;(serialized-exchange a1 a2) and (serialized-exchange a3 a1) may not occur simultaneously
;since a1 participates in both the exchanges. In such circumstances, the exchange
;processes may be analyzed individually and a generic argument may be devised. Suppose
;that there are two bank accounts acc1 and acc2 with balances a and b, respectively.
;Exchanging balances imposes the calculation of the respective difference, that is a-b.
;Withdrawing the difference from acc1 results in a-(a-b) = b. Depositing the difference
;to acc2 results in b+(a-b) = a. That is to say that, after any given transaction, the
;amounts are preserved. Hence, "if the processes are run sequentially, after any number
;of concurrent exchanges, the account balances should be maintained in some order".

;Draw a timing diagram to show how the previous condition can be violated if the
;exchanges are implemented using 'exchange'.
;---
;Assume 3 accounts: acc1 with balance $10, acc2 with balance $20 and acc3 with balance
;$30. Below, a valid sequence of actions is shown to demonstrate that serializing bank
;operations is not enough to guarantee error-free exchanges.
;---
;(parallel-execute (lambda () (exchange acc2 acc1))
;                  (lambda () (exchange acc3 acc1)))
;---
;DIFF acc2, acc1 -> $10              |
;DIFF acc3, acc1 -> $20              |
;WITH acc2       -> $10 ($20 - $10)  |
;DEPO acc1       -> $20 ($10 + $10)  |
;WITH acc3       -> $10 ($30 - $20)  |
;DEPO acc1       -> $40 ($20 + $20)  ↓ time
;---
;Note that the bank account balances were not preserved: instead of having balances of
;$10, $20 and $30 we observe balances of $10, $10 and $40. Nonetheless, the sum of the
;balances in the accounts remains still. May this be the case no matter the sequence of
;exchanges?

;On the other hand, argue that even with this exchange program ['exchange'], the sum of
;the balances in the accounts will be preserved.
;---
;The cause of the previous issue (i.e., the lack of preservation of individual balances)
;is that the computation of the difference between account balances may interleave an
;ongoing exchange process that is to set at least one of the account balances
;participating on the former calculation. This obviously hampers the preservation of
;individual balances, since the balance(s) being set at the ongoing exchange process
;would affect the result of the former difference. However, even in such a state of
;affairs, the sum of the balances of the accounts would by no means be affected. This is
;the case, since, whatever the circumstance, the amount that is withdrawn from one
;account always corresponds to the amount being deposited in the further account
;participating in the exchange. From this, results that the total balance of the accounts
;is preserved no matter the sequence of exchanges and how the respective operations may
;be interleaved.

;Draw a timing diagram to show how even this condition [preservation of the sum of the
;account balances] would be violated if we did not serialize the transactions on
;individual accounts.
;---
;To fully grasp the issue presented here, we must consider which operations, if left
;unserialized, would possibly lead to the catastrophic effect of not preserving the sum
;of account balances upon concurrent exchanges. These operations clearly correspond to
;withdrawing from and deposit to individual accounts. The timing diagram below, assumes
;account objects that do not serialize such operations.
;---
;DIFF a2 a1 -> 10         [diff in     (exchange a2 a1)]  |
;DIFF a3 a1 -> 20         [diff in     (exchange a3 a1)]  |
;BLNC a2    -> 20         [withdraw in (exchange a2 a1)]  |
;SET! a2    -> 10 (20-10) [withdraw in (exchange a2 a1)]  |
;BLNC a3    -> 30         [withdraw in (exchange a3 a1)]  |
;SET! a3    -> 10 (30-20) [withdraw in (exchange a3 a1)]  |
;BLNC a1    -> 10         [deposit in  (exchange a2 a1)]  |
;BLNC a1    -> 10         [deposit in  (exchange a3 a1)]  |
;SET! a1    -> 30 (10+20) [deposit in  (exchange a3 a1)]  |
;SET! a1    -> 20 (10+10) [deposit in  (exchange a2 a1)]  ↓ time

