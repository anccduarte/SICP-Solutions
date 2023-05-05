
#lang sicp

;------------------------------------------------------------------------------------------
;HALF-INTERVAL METHOD
;---
;let 'a' and 'b' be two x coordinates in R^2. also, let 'f' be a continuous function in
;the interval [min(a,b), max(a,b)]. assuming that f(a)<0 and f(b)>0, we may find a root
;of 'f' in [min(a,b), max(a,b)] as follows:
;---
;define g(f a b):
;  m = (a+b)/2
;  if abs(b-a) < tol: return m
;  else:
;    fm = f(m)
;    if positive(fm): return g(f a m)
;    if negative(fm): return g(f m b)
;    else: return m
;---
;at every step, the interval [a,b] is halved and it is ensured that a*b < 0. if the size
;of the interval becomes too small (less than a pre-specified tolerance) or f(m) is
;evaluated to 0, the process stops and m is returned
;------------------------------------------------------------------------------------------

;helper procedures
;---
(define average (lambda (a b) (/ (+ a b) 2.0)))
;---
(define (close-enuf? a b) (< (abs (- b a)) 0.0001))
;---
(define positive? (lambda (x) (> x 0)))
;---
(define negative? (lambda (x) (< x 0)))

;'search' -> finds a root of f in [min(neg-point,pos-point), max(neg-point,pos-point)]
;assuming that f is continuous in [min(neg-point,pos-point), max(neg-point,pos-point)],
;and that (f neg-point) < 0 and (f pos-point) > 0
;---
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enuf? neg-point pos-point)
        midpoint
        (let ((mid-val (f midpoint)))
          (cond ((positive? mid-val)
                 (search f neg-point midpoint))
                ((negative? mid-val)
                 (search f midpoint pos-point))
                (else
                 midpoint))))))

;'half-interval' -> wrapper for 'search' (checks for the validity of the arguments
;'neg-point' and 'pos-point')
;---
(define (half-interval f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (negative? a-val) (positive? b-val))
           (search f a b))
          ((and (negative? b-val) (positive? a-val))
           (search f b a))
          (else
           (error "Values are not of opposite sign"
                  a-val
                  b-val)))))

;test 'half-interval'
;---
(define f (lambda (x) (- (* x x) 1)))
;---
(half-interval f 0 2)
(half-interval f 2 0)
;---
;(half-interval f -1 1)
;(half-interval f -2 2)

