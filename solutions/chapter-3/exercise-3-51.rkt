
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.51.
;---
;In order to take a closer look at delayed evaluation, we will use the following
;procedure, which simply returns its argument after printing it:
;---
;(define (show x)
;  (display-line x)
;  x)
;---
;What does the interpreter print in response to evaluating each expression in the
;following sequence?
;---
;(define x (stream-map show
;                      (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;DEFINING STREAM PROCEDURES -> help visualization
;------------------------------------------------------------------------------------------

;'stream-enumerate-interval'
;---
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons low
                   (stream-enumerate-interval (+ low 1) high))))

;'stream-map'
;---
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

;'stream-ref'
;---
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


;------------------------------------------------------------------------------------------
;EVALUATING 'x'
;------------------------------------------------------------------------------------------

(define x (stream-map show
                      (stream-enumerate-interval 0 10)))
;---
(stream-map show (stream-enumerate-interval 0 10))
(stream-cons (show 0) (stream-map show (stream-enumerate-interval 1 10)))
;---
;0 [display]
;(stream-cons 0 (stream-map show (stream-enumerate-interval 1 10))) [return]


;------------------------------------------------------------------------------------------
;EVALUATING 1st 'stream-ref'
;------------------------------------------------------------------------------------------

(stream-ref x 5)
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 1 10))
            (- 5 1))
;---
(stream-ref (stream-cons (show 1)
                         (stream-map show
                                     (stream-enumerate-interval 2 10)))
            4)
;---
;memoize result -> (stream-cons 1 (stream-map (...)))
;1 [display]
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 2 10))
            (- 4 1))
;---
(stream-ref (stream-cons (show 2)
                         (stream-map show
                                     (stream-enumerate-interval 3 10)))
            3)
;---
;memoize result -> (stream-cons 2 (stream-map (...)))
;2 [display]
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 3 10))
            (- 3 1))
;---
(stream-ref (stream-cons (show 3)
                         (stream-map show
                                     (stream-enumerate-interval 4 10)))
            2)
;---
;memoize result -> (stream-cons 3 (stream-map (...)))
;3 [display]
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 4 10))
            (- 2 1))
;---
(stream-ref (stream-cons (show 4)
                         (stream-map show
                                     (stream-enumerate-interval 5 10)))
            1)
;---
;memoize result -> (stream-cons 4 (stream-map (...)))
;4 [display]
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 5 10))
            (- 1 1))
;---
(stream-ref (stream-cons (show 5)
                         (stream-map show
                                     (stream-enumerate-interval 6 10)))
            0)
;---
;memoize result -> (stream-cons 5 (stream-map (...)))
;5 [display]
;---
(stream-car (stream-cons 5
                         (stream-map show
                                     (stream-enumerate-interval 6 10))))
;---
;5 [return]


;------------------------------------------------------------------------------------------
;EVALUATING 2nd 'stream-ref'
;------------------------------------------------------------------------------------------

(stream-ref x 7)
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 1 10))
            (- 7 1))
;---
(stream-ref (stream-cons 1 ;memoized in 1st call to 'stream-ref'
                         (stream-map show
                                     (stream-enumerate-interval 2 10)))
            6)
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 2 10))
            (- 6 1))
;---
(stream-ref (stream-cons 2 ;memoized in 1st call to 'stream-ref'
                         (stream-map show
                                     (stream-enumerate-interval 3 10)))
            5)
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 3 10))
            (- 5 1))
;---
(stream-ref (stream-cons 3 ;memoized in 1st call to 'stream-ref'
                         (stream-map show
                                     (stream-enumerate-interval 4 10)))
            4)
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 4 10))
            (- 4 1))
;---
(stream-ref (stream-cons 4 ;memoized in 1st call to 'stream-ref'
                         (stream-map show
                                     (stream-enumerate-interval 5 10)))
            3)
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 5 10))
            (- 3 1))
;---
(stream-ref (stream-cons 5 ;memoized in 1st call to 'stream-ref'
                         (stream-map show
                                     (stream-enumerate-interval 6 10)))
            2)
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 6 10))
            (- 2 1))
;---
(stream-ref (stream-cons (show 6)
                         (stream-map show
                                     (stream-enumerate-interval 7 10)))
            1)
;---
;memoize result -> (stream-cons 6 (stream-map (...)))
;6 [display]
;---
(stream-ref (stream-map show
                        (stream-enumerate-interval 7 10))
            (- 1 1))
;---
(stream-ref (stream-cons (show 7)
                         (stream-map show
                                     (stream-enumerate-interval 8 10)))
            0)
;---
;memoize result -> (stream-cons 7 (stream-map (...)))
;7 [display]
;---
(stream-car (stream-cons 7
                         (stream-map show
                                     (stream-enumerate-interval 8 10))))
;---
;7 [return]


;------------------------------------------------------------------------------------------
;CONCLUSION
;------------------------------------------------------------------------------------------

;'x'
;---
;0 [display]
;(stream-cons 0 (stream-map show (stream-enumerate-interval 1 10))) [return]

;(stream-ref x 5)
;---
;1 [display]
;2 [display]
;3 [display]
;4 [display]
;5 [display]
;5 [return]

;(stream-ref x 7)
;---
;6 [display]
;7 [display]
;7 [return]

