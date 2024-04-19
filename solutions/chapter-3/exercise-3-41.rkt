
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.41.
;---
;Ben Bitdiddle worries that it would be better to implement the bank account as follows
;(where the commented line has been changed) [SEE BELOW *] because allowing unserialized
;access to the bank balance can result in anomalous behavior. Do you agree? Is there any
;scenario that demonstrates Ben's concern? 
;------------------------------------------------------------------------------------------

;(*) Ben's bank account implementation
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
            ((eq? m 'balance) (protected (lambda () balance)))
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    ;---
    dispatch))

;selector for 'balance' and mutators
;---
(define (get-balance account) (account 'balance))
;---
(define (withdraw account amount)
  ((account 'withdraw) amount))
;---
(define (deposit account amount)
  ((account 'deposit) amount))

;Is there any good reason for protecting the access to 'balance'?
;---
;The local variable 'balance' does not require any protection. That is to say that the
;respective selector (i.e., 'get-balance') does not need to be serialized. This is the
;case since, clearly, such an operation does not involve any assignment operation: it
;simply provides a view of the current value of the local variable 'balance'. Concurrent
;calls to 'get-balance' and to any of the account mutators may affect the value returned
;by 'get-balance'. However, such a circumstance is not avoided by serializing the latter
;procedure. Moreover, revealing the value of 'balance' before withdrawing or depositing
;from/to the bank account is just as valid as doing so after any of these operations.
;Finally, the outcome of setting 'balance' to its new value by either of the mutators is
;not affected by concurrent call(s) to 'get-balance' (either serialized or not).

