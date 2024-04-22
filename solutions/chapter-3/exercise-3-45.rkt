
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.45.
;---
;Louis Reasoner thinks our bank-account system is unnecessarily complex and error-prone
;now that deposits and withdrawals aren't automatically serialized. He suggests that
;'make-account-and-serializer' should have exported the serializer (for use by such
;procedures as 'serialized-exchange') in addition to (rather than instead off) using it
;to serialize accounts and deposits as 'make-account' did. He proposes to redefine
;accounts as follows [SEE BELOW *]. Then deposits are handled as with the original
;'make-account':
;---
;(define (deposit account amount)
;  ((account 'deposit) amount))
;---
;Explain what is wrong with Louis's reasoning. In particular, consider what happens when
;'serialized-exchange' is called.
;------------------------------------------------------------------------------------------

;'exchange'
;---
(define (exchange acc1 acc2)
  (let ((difference (- (acc1 'balance)
                       (acc2 'balance))))
    ((acc1 'withdraw) difference)
    ((acc2 'deposit) difference)))

;'serialized-exchange'
;---
(define (serialized-exchange acc1 acc2)
  (let ((s1 (acc1 'serializer))
        (s2 (acc2 'serializer)))
    ((s1 (s2 exchange)) acc1 acc2)))

;(*) Reasoner's implementation of 'make-account'
;---
(define (make-account-and-serializer balance)
  ;---
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  ;---
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ;---
  (let ((balance-serializer (make-serializer)))
    ;---
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    ;---
    dispatch))

;Explain what is wrong with Louis's reasoning. In particular, consider what happens when
;'serialized-exchange' is called.
;---
;Reasoner's suggestion, although tempting, is faulty. Note that utilizing Reasoner's
;version of 'make-account' forces 'exchange' to exploit the serialized forms of both
;'withdraw' and 'deposit'. This is alarming as, in such a context, 'serialized-exchange'
;serializes a partially serialized procedure (i.e., 'exchange'). As suggested, to fully
;comprehend why this is plainly wrong, consider the chronology of serialization imposed
;upon calling 'serialized-exchange'. Firstly, both 's1' and 's2' serialize 'exchange' (so
;far so good). The resulting serialized procedure is then applied to the accounts from
;which the serializers were exported, triggering the execution of a computational process
;running the procedure in sets 's1' and 's2'. This means that until this process finishes
;executing no other process serialized under 's1' or 's2' may be carried off. The trouble
;is that the execution of the former process depends on the application of procedures
;that are serialized under the same two sets (i.e., 's1' and 's2'), namely 'withdraw' and
;'deposit'. Since these processes may not be carried off simultaneously, the call to
;'serealized-exchange' is halted, possibly resulting in an error raise [the outcome is
;dependent on the implementation of 'make-serializer'].

