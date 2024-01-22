
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.7.
;---
;Consider the bank account objects created by 'make-account', with the password
;modification described in Exercise 3.3. Suppose that our banking system requires the
;ability to make joint accounts. Define a procedure 'make-joint' that accomplishes this.
;'make-joint' should take three arguments. The first is a password-protected account. The
;second argument must match the password with which the account was defined in order for
;the 'make-joint' operation to proceed. The third argument is a new password.
;'make-joint' is to create an additional access to the original account using the new
;password. For example, if 'peter-acc' is a bank account with password 'open-sesame',
;then
;---
;(define paul-acc
;  (make-joint peter-acc 'open-sesame 'rosebud))
;---
;will allow one to make transactions on 'peter-acc' using the name 'paul-acc' and the
;password 'rosebud'. You may wish to modify your solution to Exercise 3.3 to accommodate
;this new feature.
;------------------------------------------------------------------------------------------

;USEFUL INSIGHTS from the book (3.1.3. The Costs of Introducing Assignment - "Sameness
;and change" and "Pitfalls of imperative programming")
;---
;"A language that supports the concept that 'equals can be substituted for equals' in an
;expression without changing the value of the expression is said to be referentially
;transparent. Referential transparency is violated when we include 'set!' in our computer
;language. This makes it tricky to determine when we can simplify expressions by
;substituting equivalent expressions. Consequently, reasoning about programs that use
;assignment becomes drastically more difficult."
;---
;"In general, so long as we never modify data objects, we can regard a compound data
;object to be precisely the totality of its pieces. For example, a rational number is
;determined by giving its numerator and its denominator. But this view is no longer valid
;in the presence of change, where a compound data object has an 'identity' that is
;something different from the pieces of which it is composed. A bank account is still
;'the same' bank account even if we change the balance by making a withdrawal;
;conversely, we could have two different bank accounts with the same state information.
;This complication is a consequence, not of our programming language, but of our
;perception of a bank account as an object. We do not, for example, ordinarily regard a
;rational number as a changeable object with identity, such that we could change the
;numerator and still have 'the same' rational number."
;---
;"[...] it is ironic that introductory programming is most often taught in a highly
;imperative style. This may be a vestige of a belief, common throughout the 1960s and
;1970s, that programs that call procedures must inherently be less efficient than
;programs that perform assignments. Alternatively it may reflect a view that step-by-step
;assignment is easier for beginners to visualize than procedure call. Whatever the
;reason, it often saddles beginning programmers with 'should I set this variable before
;or after that one' concerns that can complicate programming and obscure the important
;ideas."

;SOLUTION -> 'in-list?', 'make-account' and 'make-joint'
;---
(define (in-list? elem lst)
  (cond ((null? lst) #f)
        ((eq? elem (car lst)) #t)
        (else (in-list? elem (cdr lst)))))
;---
(define (make-account balance password)
  ;---
  (let ((pass-list (list password)))
    ;---
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    ;---
    (define (withdraw amount)
      (if (<= amount balance)
          (begin (set! balance (- balance amount))
                 balance)
          (begin (display "Insufficient funds!")
                 (newline))))
    ;---
    (define (add-joint-pass pass)
      (set! pass-list (append pass-list (list pass))))
    ;---
    (lambda (pass action)
      (if (in-list? pass pass-list)
          (cond ((eq? action 'deposit) deposit)
                ((eq? action 'withdraw) withdraw)
                ((eq? action 'add-pass) add-joint-pass)
                (else (lambda (x)
                        (display "Unknown action!") (newline))))
          (lambda (x)
            (display "Incorrect password!") (newline))))))
;---
(define (make-joint acc old-pass new-pass)
  ((acc old-pass 'add-pass) new-pass)
  acc)


;TEST 'make-account' and 'make-joint'
;---
(define peter-acc (make-account 1000 'open-sesame))
;---
(define bad-paul-acc
  (make-joint peter-acc 'other-pass 'rosebud)) ;Incorrect password!
;---
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
;---
((peter-acc 'open-sesame 'deposit) 500) ;1500
((peter-acc 'rosebud 'withdraw) 200) ;1300
((paul-acc 'rosebud 'deposit) 300) ;1600
((peter-acc 'other-pass 'withdraw) 2000) ;Incorrect password!
((peter-acc 'rosebud 'withdraw) 700) ;900
((paul-acc 'open-sesame 'deposit) 0) ;900

