
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.30.
;---
;The 'sum' procedure above (SEE BELOW *) generates a linear recursion. The procedure can
;be rewritten so that the sum is performed iteratively. Show how to do this by filling in
;the missing expressions in the following definition (SEE BELOW **).
;------------------------------------------------------------------------------------------

;(*) 'sum' procedure -> recursive version
;---
(define (sum-rec term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-rec term (next a) next b))))

;(**) fill in the blanks in the following definition
;---
;(define (sum term a next b)
;  (define (iter a result)
;    (if ⟨??⟩
;        ⟨??⟩
;        (iter ⟨??⟩ ⟨??⟩)))
;  (iter ⟨??⟩ ⟨??⟩))

;ANSWER to exercise
;---
(define (sum-iter term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (+ res (term a)))))
  (iter a 0))

;test both procedurees for simple sum and compare
;---
(define identity (lambda (x) x))
(define next (lambda (x) (inc x)))
;---
(sum-rec identity 1 next 10)
(sum-iter identity 1 next 10)

