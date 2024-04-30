
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.48.
;---
;Explain in detail why the deadlock-avoidance method described above, (i.e., the accounts
;are numbered, and each process attempts to acquire the smaller-numbered account first)
;avoids deadlock in the exchange problem. Rewrite 'serialized-exchange' to incorporate
;this idea. (You will also need to modify 'make-account' so that each account is created
;with a number, which can be accessed by sending an appropriate message.)
;------------------------------------------------------------------------------------------

;Explain in detail why the deadlock-avoidance method [bank accounts are numbered] avoids
;deadlock in the exchange problem.
;---
(define (exchange acc1 acc2)
  (let ((diff (- (acc1 'balance)
                 (acc2 'balance))))
    ((acc1 'withdraw) diff)
    ((acc2 'deposit) diff)
    'ok))
;---
(define (old-serialized-exchange acc1 acc2)
  (let ((s1 (acc1 'serializer))
        (s2 (acc2 'serializer)))
    ((s1 (s2 exchange)) acc1 acc2)))
;---
;Consider 'old-serialized-exchange'. Suppose that Peter and Paul share two accounts a1
;and a2. Also, assume that Peter and Paul try to concurrently swap balances between a1
;and a2, and a2 and a1, respectively. Getting the serializers and doubly serializing
;'exchange' would constitute no incovenience. The trouble starts when the doubly
;serialized procedure is called concurrently by both clients. Let's take a look at the
;entrails of the calls and how they evolve with time.
;---
;Peter               Paul
;(mutex-1 'acquire)  (mutex-2 'acquire)  |
;(mutex-2 'acquire)  (mutex-1 'acquire)  |
;(exchange a1 a2)    (exchange a2 a1)    |
;(mutex-2 'release)  (mutex-1 'release)  |
;(mutex-1 'release)  (mutex-2 'release)  ↓ time
;---
;Observe the 2nd action of both processes. While Peter is hopelessly trying to acquire
;'mutex-2' (Paul has already acquired it), Paul is worthlessly attempting to acquire
;'mutex-1' (that Peter previously acquired). We reach an impasse: neither Peter nor Paul
;may proceed executing the exchange.
;---
;A viable solution to the problem at hand consists in numbering the bank accounts, and
;serializing exchange so that the "outer" serializer is the one exported from the
;higher-numbered account. Suppose that a1's number is larger than a2's. The concurrent
;processes would evolve as follows [assume distinct time arrows for Peter and Paul's
;actions].
;---
;Peter               Paul
;(mutex-1 'acquire)  (mutex-1 'acquire)  |
;(mutex-2 'acquire)  (mutex-2 'acquire)  |
;(exchange a1 a2)    (exchange a2 a1)    |
;(mutex-2 'release)  (mutex-2 'release)  |
;(mutex-1 'release)  (mutex-1 'release)  ↓ time
;---
;Note that, now, any exchange must be fully executed before any other concurrent exchange
;takes place. In our example, if Peter and Paul try to acquire 'mutex-1' simultaneously
;and Peter acquires it first, Paul has to wait until Peter finishes the exchange (after
;which, both 'mutex-1' and 'mutex-2' are released) to be able to acquire 'mutex-1' and
;proceed with the respective exchange operation.

;Serializers
;---
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
;---
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))
;---
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
;---
(define (clear! cell)
  (set-car! cell false))

;Bank accounts
;[All accounts have a shared variable 'current-number' which is initialized to 1 as the
;procedure is evaluated. Any time an account is created, it is numbered with the current
;value of 'current-number' and the latter is incremented by one.]
;---
(define make-account
  (let ((current-number 1))
    (lambda (balance)
      (let ((account-number current-number))
        (define (withdraw amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds!"))
        (define (deposit amount)
          (set! balance (+ balance amount))
          balance)
        (let ((serializer (make-serializer)))
          (define (dispatch m)
            (cond ((eq? m 'balance) balance)
                  ((eq? m 'account-number) account-number)
                  ((eq? m 'serializer) serializer)
                  ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)))
          (set! current-number (+ current-number 1))
          dispatch)))))

;New serialized exchange
;[The order by which 'exchange' is serialized matters. The "outer" serializer corresponds
;to the one exported from the account whose number is larger.]
;---
(define (serialized-exchange acc1 acc2)
  (let ((s1 (acc1 'serializer))
        (s2 (acc2 'serializer)))
    (if (< (acc1 'account-number) (acc2 'account-number))
        ((s2 (s1 exchange)) acc1 acc2)
        ((s1 (s2 exchange)) acc1 acc2))))

