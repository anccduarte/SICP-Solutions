
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.42.
;---
;Ben Bitdiddle suggests that it's a waste of time to create a new serialized procedure in
;response to every withdraw and deposit message. He says that 'make-account' could be
;changed so that the calls to 'protected' are done outside the dispatch procedure. That
;is, an account would return the same serialized procedure (which was created at the same
;time as the account) each time it is asked for a withdrawal procedure [SEE BELOW *]. Is
;this a safe change to make? In particular, is there any difference in what concurrency
;is allowed by these two versions of 'make-account'?
;------------------------------------------------------------------------------------------

;Original version of 'make-account'
;---
(define (make-account balance)
  ;---
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds!"))
  ;---
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;---
  (let ((protected (make-serializer)))
    ;---
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    ;---
    dispatch))

;(*) Ben's suggestion
;---
(define (bens-make-account balance)
  ;---
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  ;---
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;---
  (let ((protected (make-serializer)))
    ;---
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      ;---
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))
      ;---
      dispatch)))

;Is there any difference in what concurrency is allowed by these two versions of
;'make-account'?
;---
(let ((acc (make-account)))
  (parallel-execute (lambda () ((acc 'withdraw) 10))
                    (lambda () ((acc 'withdraw) 20)))
  (acc 'balance))
;---
;Take the above test as an example. Considering the original implementation of bank
;accounts, the expression would indubitably return the value 70. Both withdrawing
;operations would be serialized upon the respective calls, and non-interleaved execution
;would be ensured. The value of 'balance' would evolve in either one of: 100 -> 90 -> 70
;or 100 -> 80 -> 70. Obviously, this is the expected outcome, and does not leave any room
;for putative pernicious effects of concurrent computataion. On the other hand, by
;serializing 'withdraw' and 'deposit' in the environment enclosing 'dispatch' and then
;returning these procedures when appropriate, raises some hesitations regarding the
;possible outcomes of parallel execution of the respective operations. Taking, again, the
;above test as an example, we would essentially be parallelizing the execution of the
;same procedure. Since both 'parallel-execute' and 'make-serializer' implementations are
;undisclosed at this point in time, it is impossible to determine the behavior of the
;former ragarding the processing of duplicate procedures, that is, knowing whether
;interleaving is allowed in such cases is beyond our current understanding. In such
;circumstances, lurking nocive effects of concurrency suddenly become apparent (e.g., the
;above test might produce three distinct outcomes: 70, 80 or 90).

