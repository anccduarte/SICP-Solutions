
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.11.
;---
;A function 'f' is defined by the rule that
;f(n) = n if n<3
;       f(n-1) + 2*f(n-2) + 3*f(n-3) if n>=3
;Write a procedure that computes 'f' by means of a recursive process. Write a procedure
;that computes 'f' by means of an iterative process.
;------------------------------------------------------------------------------------------


;---
;SIMPLER CASE - FIBONACCI
;---

;fibo-rec -> originates recursive process
;---
(define (fibo-rec n)
  (if (< n 2)
      n
      (+ (fibo-rec (dec n)) (fibo-rec (- n 2)))))

;fibo-iter -> originates iterative process
;to compute fibo(n) -> a=0, b=1, c=0; while c<n: a=b, b=a+b; return a
;---
(define (fibo-iter n)
  (define (iter a b count)
    (if (= count n)
        a
        (iter b (+ a b) (inc count))))
  (iter 0 1 0))

;test 'fibo-rec' and 'fibo-iter'
;---
(fibo-rec 5)
(fibo-iter 5)


;---
;ANSWER TO EXERCISE
;---

;procedure computing 'f' by means of a recursive process
;---
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (dec n))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

;procedure computing 'f' by means of an iterative process
;within 'f-iter', define a procedure 'iter' with 4 formal parameters: a, b, c and count
;a=0, b=1, c=2, count=0; while count<n: a=b, b=c, c=3*a+2*b+c; return a
;below is an example for n=6 (it would actually compute f(n) up to n=8 to return f(6);
;this is done to ensure that f(0) and f(1) have an answer)
;---
;0  1  2  3  4  5  6
;0  1  2  4  11 25 59
;a  b  c
;   a  b  c
;      a  b  c
;         a  b  c
;            a  b  c
;---
(define (f-iter n)
  (define (iter a b c count)
    (if (= count n)
        a
        (iter b
              c
              (+ (* 3 a)
                 (* 2 b)
                 c)
              (inc count))))
  (iter 0 1 2 0))

;test 'f-rec' and 'f-iter'
;---
(f-rec 6)
(f-iter 6)

