
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.5.
;---
;'Monte Carlo integration' is a method of estimating definite integrals by means of Monte
;Carlo simulation. Consider computing the area of a region of space described by a
;predicate P(x, y) that is true for points (x, y) in the region and false for points not
;in the region. For example, the region contained within a circle of radius 3 centered at
;(5, 7) is described by the predicate that tests whether (x-5)^2 + (y-7)^2 <= 3^2. To
;estimate the area of the region described by such a predicate, begin by choosing a
;rectangle that contains the region. For example, a rectangle with diagonally opposite
;corners at (2, 4) and (8, 10) contains the circle above. The desired integral is the
;area of that portion of the rectangle that lies in the region. We can estimate the
;integral by picking, at random, points (x, y) that lie in the rectangle, and testing
;P(x, y) for each point to determine whether the point lies in the region. If we try this
;with many points, then the fraction of points that fall in the region should give an
;estimate of the proportion of the rectangle that lies in the region. Hence, multiplying
;this fraction by the area of the entire rectangle should produce an estimate of the
;integral.
;---
;Implement Monte Carlo integration as a procedure 'estimate-integral' that takes as
;arguments a predicate 'P', upper and lower bounds 'x1', 'x2', 'y1', and 'y2' for the
;rectangle, and the number of trials to perform in order to produce the estimate. Your
;procedure should use the same 'monte-carlo' procedure that was used above to estimate
;'pi'. Use your 'estimate-integral' to produce an estimate of 'pi' by measuring the area
;of a unit circle.
;---
;You will find it useful to have a procedure that returns a number chosen at random from
;a given range. The following 'random-in-range' procedure implements this in terms of the
;'random' procedure used in Section 1.2.6, which returns a nonnegative number less than
;its input.
;---
;(define (random-in-range low high)
;  (let ((range (- high low)))
;    (+ low (random range))))
;------------------------------------------------------------------------------------------

;'estimate-pi'
;---
(define (estimate-pi trials)
  (estimate-integral (lambda (x y)
                       (<= (+ (* x x) (* y y)) 1))
                     -1
                     1
                     -1
                     1
                     trials))

;'estimate-integral'
;('points-inside' represents the percentage of random points that passed the test imposed
;by the predicate 'P' -> to attain the wanted area, i.e., the region delimited by 'P',
;'points-inside' has to be multiplied by the area of the enclosing rectangle, that is,
;'area-rectangle')
;---
(define (estimate-integral P x1 x2 y1 y2 trials)
  ;---
  (let ((points-inside (monte-carlo trials (lambda ()
                                             (P (random-in-range x1 x2)
                                                (random-in-range y1 y2)))))
        ;---
        (area-rectangle (abs (* (- x2 x1) (- y2 y1)))))
    ;---
    (* points-inside area-rectangle)))

;'random-in-range'
;(adding 0.0 to 'range' allows for the generation of random floats instead of random
;integers; this circumvents two essential issues: 1. choosing floats instead of integers
;allows for the generation a much wider set of numbers having the same lower and upper
;bounds as reference [the pernicious consequence of generating integers are not discussed
;here; however, it is important to mention that increasing the lower and/or upper bounds
;of the enclosing rectangle inflates the computed area of the circle and, consequently,
;results in an enlargement of the value of pi]; 2. avoids a rather furtive bug - instead
;of generating a number in the interval [low, high], 'random-in-range' generates a
;number belonging to [low, high) [adding 0.0 to 'range' still produces a float belonging
;to the latter interval, although the consequences of such a persisting aspect are not as
;nefarious: e.g., calling the original 'random-in-range' with arguments -1 and 1 either
;produces -1 or 0, whereas calling the ongoing version with the same arguments yields
;floats ranging from -1 to 0.9999999999999999])
;---
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low
       (random (+ range 0.0)))))

;'monte-carlo'
;---
(define (monte-carlo trials experiment)
  ;---
  (define (monte-carlo-iter num-exps num-passed)
    (cond ((= num-exps trials)
           (/ num-passed trials))
          ((experiment)
           (monte-carlo-iter (+ num-exps 1)
                             (+ num-passed 1)))
          (else
           (monte-carlo-iter (+ num-exps 1)
                             num-passed))))
  ;---
  (monte-carlo-iter 0 0))

;test 'estimate-pi'
;---
(exact->inexact (estimate-pi 100000))

