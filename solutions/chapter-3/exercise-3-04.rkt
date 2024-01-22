
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.4.
;---
;Modify the 'make-account' procedure of Exercise 3.3 by adding another local state
;variable so that, if an account is accessed more than seven consecutive times with an
;incorrect password, it invokes the procedure 'call-the-cops'.
;------------------------------------------------------------------------------------------

;reinforced protection on 'make-account'
;---
(define (make-account balance password)
  ;---
  (let ((count 0) (limit 7))
    ;---
    (define (withdraw amount)
      (if (>= amount balance)
          (begin
            (display "Insufficient funds!")
            (newline))
          (begin
            (set! balance (- balance amount))
            balance)))
    ;---
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    ;---
    (define (call-the-cops)
      (display "Calling the cops!") (newline))
    ;---
    (define (dispatch p m)
      (if (eq? p password)
          (begin
            (set! count 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else
                   (lambda (x)
                     (display "Unknown request!") (newline)))))
          (lambda (x)
            (display "Incorrect password!") (newline)
            (set! count (+ count 1))
            (if (>= count limit) (call-the-cops) (display "")))))
    ;---
    dispatch))

;test 'make-account'
;---
(define (try-access acc n)
  (define (access-iter c)
    (if (> c n)
        (display "")
        (begin
          ((acc 'wrong-password 'unknown-request) n)
          (access-iter (+ c 1)))))
  (access-iter 1))
;---
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ;60
((acc 'some-other-password 'deposit) 50) ;"Incorrect password!"
((acc 'secret-password 'other) 50) ;"Unknown request!"
;---
(display "---") (newline)
;---
(try-access acc 8)

