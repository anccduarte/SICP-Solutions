
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.74.
;---
;Alyssa P. Hacker is designing a system to process signals coming from physical sensors.
;One important feature she wishes to produce is a signal that describes the zero
;crossings of the input signal. That is, the resulting signal should be +1 whenever the
;input signal changes from negative to positive, 1 whenever the input signal changes from
;positive to negative, and 0 otherwise. (Assume that the sign of a 0 input is positive.)
;For example, a typical input signal with its associated zero-crossing signal would be
;---
;... 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 ...
;... 0 0  0  0  0   -1   0  0  0   0   1  0 0 ...
;---
;In Alyssa's system, the signal from the sensor is represented as a stream 'sense-data'
;and the stream 'zero-crossings' is the corresponding stream of zero crossings. Alyssa
;first writes a procedure 'sign-change-detector' that takes two values as arguments and
;compares the signs of the values to produce an appropriate 0, 1, or - 1. She then
;constructs her zero-crossing stream as follows [see below *]. Alyssa's boss, Eva Lu
;Ator, walks by and suggests that this program is approximately equivalent to the
;following one, which uses the generalized version of 'stream-map' from Exercise 3.50:
;---
;(define zero-crossings
;  (stream-map sign-change-detector
;              sense-data
;              <expression>))
;---
;Complete the program by supplying the indicated <expression.
;------------------------------------------------------------------------------------------

;(*) Alyssa's 'zero-crossings'
;---
(define alyssa-zero-crossings
  (make-zero-crossings sense-data 0))
;---
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))

;Ator's 'zero-crossings'
;---
;Note the more generic version of 'stream-map'. Such a variant takes the "cars" of the
;input streams, applies the procedure to these "cars", and "cons" them onto the procedure
;mapping of the "cdrs" of the streams. Mapping 'sign-change-detector' to the input signal
;taken as example and to the respective "cdr" results in the following:
;---
;1  2    1.5  1     0.5  -0.1  -2  -3  -2    -0.5  0.2  3  4    =  example
;2  1.5  1    0.5  -0.1  -2    -3  -2  -0.5   0.2  3    4  ...  =  cdr example
;0  0    0    0    -1     0     0   0   0     1    0    0  ...  =  map
;---
(define ator-zero-crossings
  (stream-map sign-change-detector
              sense-data
              (stream-cdr sense-data)))

;meteorgan's implementation
;[http://community.schemewiki.org/?sicp-ex-3.74]
;---
;The previous solution is not consistent with Alyssa's implementation. Alyssa's version
;of 'zero-crossing' starts by calling 'sign-change-detector' with the "car" of the input
;stream and 0 as arguments. To reflect such a circumstance 'ator-zero-crossings' should
;have been defined as:
;---
(define ator-zero-crossings
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0
                           sense-data)))

