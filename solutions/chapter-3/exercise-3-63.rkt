
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.63.
;---
;Louis Reasoner asks why the 'sqrt-stream' procedure was not written in the following
;more straight-forward way, without the local variable 'guesses':
;---
;(define (sqrt-stream x)
;  (cons-stream 1.0 (stream-map
;                    (lambda (guess)
;                      (sqrt-improve guess x))
;                    (sqrt-stream x))))
;---
;Alyssa P. Hacker replies that this version of the procedure is considerably less
;efficient because it performs redundant computation. Explain Alyssa's answer. Would the
;two versions still differ in efficiency if our implementation of 'delay' used only
;(lambda () <exp>) without using the optimization provided by 'memo-proc' (Section 3.5.1)?
;------------------------------------------------------------------------------------------

;Original implementation of 'sqrt-stream'
;---
(define (improve-sqrt guess x)
  (define average
    (lambda (a b) (/ (+ a b) 2)))
  (average guess (/ x guess)))
;---
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (improve-sqrt guess x))
                             guesses)))
  guesses)

;Answer
;---
;Consider the evolution of the stream as 'stream-ref' is called on (sqrt-stream x). The
;process evolves similarly by following either implementation of 'sqrt-stream' [a subtle
;distinction is unveiled later].
;---
;(cons-stream 1.0
;             (stream-map (lambda ...)
;                         (cons-stream
;                          1.0
;                          (stream-map (lambda ...)
;                                      (cons-stream 1.0
;                                                   (stream-map (lambda ...)
;                                                               ...))))))
;---
;Consider the original implementation of 'stream-sqrt' and assume 'y' to be the result
;yielded by the evaluation of (stream-sqrt 4). Getting successive approximations of the
;square root of 4 is then accomplished by "cdring down" the stream 'y'. For example, if
;we want the 8th approximation of the square root of 4, we simply call (stream-ref y 8).
;The advantage of the original implementation lies precisely here: by defining a stream
;'guesses', we instantly have access to memoization, which avoids recomputation of values
;when "cdring down" the stream. Then, evaluating (stream-ref y 8) evolves a process
;similar to the one described above with a slight distinction: at every step of
;computation, the previous approximation is readily retrieved without recomputing values.
;For example, the 'stream-cddr' of 'y' is given by (improve-sqrt 2.5 4), avoiding the
;nested evaluation (improve-sqrt (improve-sqrt 1.0 4) 4) [note that (improve-sqrt 1.0 4)
;had already been computed when selecting the 'stream-cdr']. Not defining 'guesses', just
;like Reasoner suggests, impedes memoization since there is no defined stream to hold on
;to [i.e., at every step, a new/fresh stream is generated]. In conclusion, the number of
;calls to 'improve-sqrt' needed to compute the nth approximation of the square root
;assuming the original and Reasoner's versions of 'sqrt-streams' is, respectively, n and
;(partial-sum n) [assume (partial-sum n) to return 1 + 2 + ... + (n-1) + n]. Obviously,
;defining 'delay' simply as (lambda () <exp>) dissipates the differences between both
;implementations.

