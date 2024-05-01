
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.49.
;---
;Give a scenario where the deadlock-avoidance mechanism described above [i.e., the
;accounts are numbered, and each process attempts to acquire the smaller-numbered account
;first] does not work. (Hint: In the exchange problem, each process knows in advance
;which accounts it will need to get access to. Consider a situation where a process must
;get access to some shared resources before it can know which additional shared resources
;it will require.)
;------------------------------------------------------------------------------------------

;Give a scenario where the deadlock-avoidance mechanism does not work.
;---
;Consider 3 friends (Anne, Betty and Carl) sharing 3 accounts (a1, a2 and a3). Also,
;assume there exists a procedure, call it 'swap-extremes', that receives an arbitrary
;number of accounts and performs an exchange on the ones having the lower and higher
;balances via calling 'serialized-exchange' [see Exercise 3.48. for its implementation].
;---
;Initially, a1, a2 and a3 have balances 100$, 200$ and 300$, respectively. Suppose that
;the three friends interact with the accounts simultaneously in the following manner:
;- Anne deposits 150$ in account a1 and withdraws 250$ from account a3;
;- Betty exchanges the balances of the accounts with higher and lower funds [calling
;  'swap-extremes' under the hood];
;- Carl performs the same operation as Betty.
;---
;A perfectly reasonable sequence of events not violating any principle of concurrency
;would go as follows:
;1. Betty's process determines that a1 has the lowest balance and that a3 has the highest
;   balance among all shared accounts;
;2. Anne deposits 150$ in account a1, modifying the catalogue of balances to 250$, 200$
;   and 300$ (a1, a2 and a3, respectively);
;3. Anne withdraws 250$ from account a3, altering the listing of balances to 250$, 200$
;   and 50$ (a1, a2 and a3, respectively);
;4. Carl's process discovers that a3 has the lowest balance and that a1 has the highest
;   balance among all bank accounts;
;5. Betty's process calls 'serialized-exchange' on a1 and a3;
;6. Carls's process calls 'serialized-exchange' on a1 and a3.
;---
;Note that the deadlock-avoidance mechanism does not avoid an impasse upon the concurrent
;calls to 'serialized-exchange'. Although the accounts are the same (a1 and a3) their
;balances are not consistent. In fact, a1 is the lowest-funded account for Betty's
;process and the highest-funded account for Carl's process. The exact opposite happens
;for account a3. This means that the order by which the serializers are applied to
;'exchange' in 'serialized-exchange' is swapped between processes, inevitably leading to
;the difficulty the deadlock-avoidance mechanism tried to prevent. [For a detailed
;exposition of such a difficulty, see Exercise 3.48.]

