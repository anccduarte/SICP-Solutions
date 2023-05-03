
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.29.
;---
;Simpson's Rule is a more accurate method of numerical integration than the method
;illustrated above (SEE BELOW *). Using Simpson's Rule, the integral of a function 'f'
;between 'a' and 'b' is approximated as:
;(h/3) * (y0 + 4*y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-1 + yn)
;h = (b-a)/n
;yk = f(a+kh)
;(Increasing 'n' increases the accuracy of the approximation.) Define a procedure that
;takes as arguments 'f', 'a', 'b', and 'n' and returns the value of the integral,
;computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1
;(with n = 100 and n = 1000), and compare the results to those of the integral procedure
;shown above.
;------------------------------------------------------------------------------------------

;"One of the things we should demand from a powerful programming language is the ability
;to build abstractions by assigning names to common patterns and then to work in terms of
;the abstractions directly. Procedures provide this ability. This is why all but the most
;primitive programming languages include mechanisms for defining procedures."

;"Often the same programming pattern will be used with a number of different procedures.
;To express such patterns as concepts, we will need to construct procedures that can
;accept procedures as arguments or return procedures as values. Procedures that
;manipulate procedures are called higher-order procedures."

;procedure capturing the concept of sum
;---
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;(*) 'integral' in terms of 'sum'
;integral[a,b](f) = (f(a+dx/2) + f(a+dx+dx/2) + f(a+2dx+dx/2) + ...) * dx
;---
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2))
          (lambda (x) (+ x dx))
          b)
     dx))

;ANSWER to exercise
;---
;odd terms -> (sum f (+ a h) next (- b (/ h 2)))
;             (+ a h) - ignores first term
;             (- b (/ h 2)) - ignores last term
;---
;even terms -> (sum f (+ a (* 2 h)) next (- b (/ h 2)))
;              (+ a (* 2 h)) - ignores first term
;              (- b (/ h 2)) - ignores last term
;---
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define next (lambda (x) (+ x (* 2 h))))
  (* (+ (f a)
        (* 4 (sum f
                  (+ a h)
                  next
                  (- b (/ h 2))))
        (* 2 (sum f
                  (+ a (* 2 h))
                  next
                  (- b (/ h 2))))
        (f b))
     (/ h 3.0)))

;test both procedures and compare
;---
(define cube (lambda (x) (* x x x)))
;---
(display "integral") (newline)
(display "100 iterations -> ") (integral cube 0 1 0.01)
(display "1000 iterations -> ") (integral cube 0 1 0.001)
(display "10000 iterations -> ") (integral cube 0 1 0.0001)
(display "100000 iterations -> ") (integral cube 0 1 0.00001)
;---
(display "---") (newline)
(display "simpson-integral") (newline)
(display "100 iterations -> ") (simpson-integral cube 0 1 100)
(display "1000 iterations -> ") (simpson-integral cube 0 1 1000)
(display "10000 iterations -> ") (simpson-integral cube 0 1 10000)
(display "100000 iterations -> ") (simpson-integral cube 0 1 100000)

