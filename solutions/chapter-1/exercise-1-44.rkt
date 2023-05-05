
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.44.
;---
;The idea of smoothing a function is an important concept in signal processing. If 'f' is
;a function and 'dx' is some small number, then the smoothed version of 'f' is the
;function whose value at a point 'x' is the average of f(x-dx), f(x), and f(x+dx). Write
;a procedure 'smooth' that takes as input a procedure that computes 'f' and returns a
;procedure that computes the smoothed 'f'. It is sometimes valuable to repeatedly smooth
;a function (that is, smooth the smoothed function, and so on) to obtain the n-fold
;smoothed function. Show how to generate the n-fold smoothed function of any given
;function using 'smooth' and 'repeated' from Exercise 1.43.
;------------------------------------------------------------------------------------------

;defining 'smooth' -> g(x) = average(f(x-dx), f(x), f(x+dx))
;---
(define (smooth f)
  (define average
    (lambda (a b c) (/ (+ a b c) 3)))
  (let ((dx 0.00001))
    (lambda (x) (average (f (- x dx))
                         (f x)
                         (f (+ x dx))))))

;defining 'compose' -> from exercise 1.42.
;---
(define (compose f g)
  (lambda (x) (f (g x))))

;defining 'repeated' -> from exercise 1.43.
;---
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

;defining 'repeated-smooth' in terms of 'smooth' and 'repeated'
;---
(define (repeated-smooth f n)
  ((repeated smooth n) f))
;---
;EXAMPLE (n=4)
;(repeated smooth 4) -> (lambda (x) (smooth (smooth (smooth (smooth x)))))
;((repeated smooth 4) f) -> ((lambda (x) (smooth (smooth (smooth (smooth x))))) f)
;                           (smooth (smooth (smooth (smooth f))))

;testing 'repeated-smooth'
;---
(define f (lambda (x) (* x x)))
;---
(display "f(2) = ") (f 2)
;---
(display "---") (newline) (display "'Manual' smoothing") (newline)
((smooth f) 2)
((smooth (smooth f)) 2)
((smooth (smooth (smooth f))) 2)
((smooth (smooth (smooth (smooth f)))) 2)
;---
(display "---") (newline) (display "With 'repeated-smooth'") (newline)
((repeated-smooth f 1) 2)
((repeated-smooth f 2) 2)
((repeated-smooth f 3) 2)
((repeated-smooth f 4) 2)

