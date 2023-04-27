
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.18.
;---
;Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that generates
;an iterative process for multiplying two integers in terms of adding, doubling, and
;halving and uses a logarithmic number of steps.
;------------------------------------------------------------------------------------------

;helper methods
;---
(define double (lambda (x) (* x 2)))
(define halve (lambda (x) (/ x 2)))
(define even? (lambda (x) (= (remainder x 2) 0)))

;'mult' from previous exercise -> generates recursive process
;if b is even => a*b = double(a*halve(b))
;if b is odd => a*b = a+(a*(b-1))
;---
(define (mult-rec a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult-rec a (halve b))))
        (else (+ a (mult-rec a (dec b))))))

;'mult-iter' -> generates iterative process
;as in exercise 1.16., add a new state variable 'c' such that c+a*b is constant between
;procedural calls
;if b is even => c+ab = c+2*(a*(b/2)) = (c)+(2*a)*(b/2)
;if b is odd => c+ab = c+a+(a*(b-1)) = (c+a)+(a)*(b-1) 
;---
(define (mult-iter a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (dec b) (+ c a)))))
  (iter a b 0))

;test both implementations for random numbers a and b
;---
(define (test a b)
  (let ((x (mult-rec a b))
        (y (mult-iter a b)))
    (display (= x y)) (display " ")
    (display "(") (display a) (display "*") (display b) (display ") ")
    (display x) (display " ") (display y) (newline)))
;---
(test 2 4)
(test 3 3)
(test 2 19)

