
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.82.
;---
;Redo Exercise 3.5 on Monte Carlo integration in terms of streams. The stream version of
;'estimate-integral' will not have an argument telling how many trials to perform.
;Instead, it will produce a stream of estimates based on successively more trials.
;------------------------------------------------------------------------------------------

;'estimate-integral'
;---
;'monte-carlo' is fed a stream of boolean values. Hence, the tests P(x,y) are effected
;prior to the Monte Carlo simulation. The results of such tests are provided as the first
;argument to 'monte-carlo'. The Monte Carlo stream is then constructed by successively
;computing the ratio between the number experiments passed and the total number of
;experiments. Finally, successive approximations to the integral's value are attained by
;scaling the latter stream by a factor equivalent to the area of the rectangle confined
;by x1, x2, y1 and y2. For more details on Monte Carlo integration, see Exercise 3.5.
;---
(define (estimate-integral P x1 x2 y1 y2)
  ;---
  (let ((area-rectangle (* (- x2 x1) (- y2 y1)))
        (monte-carlo-stream (monte-carlo
                             (stream-map
                              P
                              (rand-range-stream x1 x2)
                              (rand-range-stream y1 y2))
                             0
                             0)))
    ;---
    (scale-stream monte-carlo-stream area-rectangle)))

;'monte-carlo'
;---
(define (monte-carlo experiment-stream passed failed)
  ;---
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo (stream-cdr experiment-stream)
                  passed
                  failed)))
  ;---
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;'random-in-range-stream'
;---
(define (rand-range-stream low high)
  ;---
  (define low-stream (cons-stream low low-stream))
  (define high-stream (cons-stream high high-stream))
  ;---
  (define (random-in-range x y)
    (let ((range (- y x)))
      (+ x (random (+ range 0.0)))))
  ;---
  (stream-map random-in-range low-stream high-stream))

