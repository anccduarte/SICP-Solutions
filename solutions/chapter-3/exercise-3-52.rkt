
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.52.
;---
;Consider the sequence of expressions
;---
;(define sum 0)
;---
;(define (accum x)
;  (set! sum (+ x sum)) sum)
;---
;(define seq (stream-map accum
;                        (stream-enumerate-interval 1 20)))
;---
;(define y (stream-filter even? seq))
;---
;(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                         seq))
;---
;(stream-ref y 7)
;(display-stream z)
;---
;What is the value of sum after each of the above expressions is evaluated? What is the
;printed response to evaluating the 'stream-ref' and 'display-stream' expressions? Would
;these responses differ if we had implemented (delay <exp>) simply as (lambda () <exp>)
;without using the optimization provided by 'memo-proc'? Explain.
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;PRELUDE
;------------------------------------------------------------------------------------------

;The rationale behind the evolution of the processes below is quite similar to the one
;exposed in Exercise 3.51. (except for the mixing of streams and assignment). For a more
;detailed exposition on the evolution of stream processes, see the solution to the latter
;exercise or review textbook's pages 435-438.


;------------------------------------------------------------------------------------------
;STREAM PROCEDURES -> help visualization
;------------------------------------------------------------------------------------------

;'stream-ref'
;---
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;'stream-map'
;---
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

;'stream-enumerate-interval'
;---
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons low
                   (stream-enumerate-interval (+ low 1) high))))

;'stream-filter'
;---
(define (stream-filter pred s)
  (cond ((stream-null? s)
         the-empty-stream)
        ((pred (stream-car s))
         (stream-cons (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else
         (stream-filter pred (stream-cdr s)))))


;------------------------------------------------------------------------------------------
;DEFINITIONS
;------------------------------------------------------------------------------------------

;Important note
;---
;The outcome of 'z' and the value of 'sum' are incorrect if we do not consider 'cdring'
;to be memoized. The nocive effects of such a mistake solely show up in the last section,
;where, upon initializing 'y' and 'z', 'sum' should evaluate to 5 instead of 3.

;'seq'
;---
(stream-map accum (stream-enumerate-interval 1 20))
(stream-cons (accum 1) (stream-map accum (stream-enumerate-interval 2 20)))
(stream-cons 1 (stream-map accum (stream-enumerate-interval 2 20)))
;---
;'sum' is set to 1
;no memoization -> so far, we solely evaluated the car of the stream

;'y'
;---
(stream-filter even? seq)
(stream-filter even? (stream-map accum (stream-enumerate-interval 2 20)))
(stream-filter even? (stream-cons (accum 2)
                                  (stream-map accum (stream-enumerate-interval 3 20))))
(stream-filter even? (stream-cons 3 ;(+ 2 1)
                                  (stream-map accum (stream-enumerate-interval 3 20))))
;---
;'sum' is set to 3
;'stream-cdr' of (stream-map accum (stream-enumerate-interval 2 20)) is memoized!

;'z'
;---
(stream-filter div5? seq)
(stream-filter div5? (stream-map accum (stream-enumerate-interval 2 20)))
(stream-filter div5? (stream-cons 3
                                  (stream-map accum (stream-enumerate-interval 3 20))))
;---
;'sum' is still 3
;'stream-cdr' of (stream-map accum (stream-enumerate-interval 2 20)) already memoized


;------------------------------------------------------------------------------------------
;CALLS to 'stream-ref' and 'display-stream' [with memoization]
;------------------------------------------------------------------------------------------

;'stream-ref'
;---
(stream-ref y 7)
;---
;'sum' set to 6 (3+3)
;'stream-ref' counter set to 6 [1st even number]
;---
;'sum' set to 10 (6+4)
;'stream-ref' counter set to 5 [2nd even number]
;---
;'sum' set to 15 (10+5)
;'stream-ref' counter still at 5
;---
;'sum' set to 21 (15+6)
;'stream-ref' counter still at 5
;---
;'sum' set to 28 (21+7)
;'stream-ref' counter set to 4 [3rd even number]
;---
;'sum' set to 36 (28+8)
;'stream-ref' counter set to 3 [4th even number]
;---
;'sum' set to 45 (36+9)
;'stream-ref' counter still at 3
;---
;'sum' set to 55 (45+10)
;'stream-ref' counter still at 3
;---
;'sum' set to 66 (55+11)
;'stream-ref' counter set to 2 [5th even number]
;---
;'sum' set to 78 (66+12)
;'stream-ref' counter set to 1 [6th even number]
;---
;'sum' set to 91 (78+13)
;'stream-ref' counter still at 1
;---
;'sum' set to 105 (91+14)
;'stream-ref' counter still at 1
;---
;'sum' set to 120 (105+15)
;'stream-ref' counter set to 0 [7th even number]
;---
;120 [return value]

;'display-stream'
;---
;Note that, for all (x,y) with x belonging to the interval [2,16], the stream represented
;by (stream-cons y (stream-map accum (stream-enumerate-interval x 20))) is memoized (the
;previous successive 'cdrings' down 'seq' operated such memoization). Hence, 'cdring'
;down the stream once again will only impact 'sum' when x>16. The following then happens:
;---
; 10 [display in REPL]
; 15 [display in REPL]
; 45 [display in REPL]
; 55 [display in REPL]
;105 [display in REPL]
;120 [display in REPL]
;---
;'sum' is set to 136 (120+16)
;'sum' is set to 153 (136+17)
;'sum' is set to 171 (153+18)
;'sum' is set to 190 (171+19)
;'sum' is set to 210 (190+20)
;---
;190 [display in REPL]
;210 [display in REPL]


;------------------------------------------------------------------------------------------
;CALLS to 'stream-ref' and 'display-stream' [without memoization]
;------------------------------------------------------------------------------------------

;'stream-ref'
;---
;Since the call to 'stream-ref' involves 'cdring' down the stream 'seq' for the first
;time, both 'sum' and the result of the call are equivalent to the ones computed in the
;previous section (120).

;'display-stream'
;---
;Not memoizing delayed expressions causes for the recalculation of all cdrs of 'seq'.
;Since 'cdring' down 'seq' involves updating 'sum' (which was already updated multiple
;times upon the call to 'stream-ref'), the yielded result is quite distinct from the
;outcome attained in the previous section.
;---
;'sum' is set to 121
;'sum' is set to 123
;'sum' is set to 126
;'sum' is set to 130
;'sum' is set to 135
;'sum' is set to 141
;'sum' is set to 148
;'sum' is set to 156
;'sum' is set to 165
;'sum' is set to 175
;'sum' is set to 186
;'sum' is set to 198
;'sum' is set to 211
;'sum' is set to 225
;'sum' is set to 240
;'sum' is set to 256
;'sum' is set to 273
;'sum' is set to 291
;'sum' is set to 310
;'sum' is set to 330
;---
;130 [display in REPL]
;135 [display in REPL]
;165 [display in REPL]
;175 [display in REPL]
;225 [display in REPL]
;240 [display in REPL]
;310 [display in REPL]
;330 [display in REPL]

