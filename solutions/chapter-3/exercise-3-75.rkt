
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.75.
;---
;Unfortunately, Alyssa's zero-crossing detector in Exercise 3.74 proves to be
;insufficient, because the noisy signal from the sensor leads to spurious zero crossings.
;Lem E. Tweakit, a hardware specialist, suggests that Alyssa smooth the signal to filter
;out the noise before extracting the zero crossings. Alyssa takes his advice and decides
;to extract the zero crossings from the signal constructed by averaging each value of the
;sense data with the previous value. She explains the problem to her assistant, Louis
;Reasoner, who attempts to implement the idea, altering Alyssa's program as follows [see
;below *]. This does not correctly implement Alyssa's plan. Find the bug that Louis has
;installed and fix it without changing the structure of the program. (Hint: You will need
;to increase the number of arguments to 'make-zero-crossings'.)
;------------------------------------------------------------------------------------------

;(*) Reasoner's implementation of 'make-zero-crossings'
;---
;Alyssa's pretension is essentially to construct a stream of averages (i.e., a stream
;whose elements are determined by averaging contiguous values of the input stream) and
;then call the sign change detector procedure on contiguous elements of this stream.
;Reasoner's procedure does not accomplish such requisites. Instead, to construct the
;stream of averages, at every step, it takes the "car" of the input stream and averages
;it with the previous average, thus, incorrectly constructing it.
;---
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-value)
     (make-zero-crossings
      (stream-cdr input-stream) avpt))))

;Variant of 'make-zero-crossings' fulfilling Alyssa's pretensions
;---
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings (stream-cdr input-stream)
                          (stream-car input-stream)
                          avpt))))

;xdavidliu's suggestion
;[http://community.schemewiki.org/?sicp-ex-3.75]
;---
;As correctly pointed out by xdavidliu, there is no need for 'make-zero-crossings' to
;accept this many arguments. In fact, it needs only to be fed the sense data stream.
;Although such an implementation makes it easier to unravel the program's workings, it
;inevitably leads to recomputation of values, and, hence, it is clearly less efficient.
;---
(define average
  (lambda (x y) (/ (+ x y) 2)))
;---
(define (make-zero-crossings sense-data)
  (let ((x1 (stream-car sense-data))
        (x2 (stream-car (stream-cdr sense-data)))
        (x3 (stream-car (stream-cdr (stream-cdr sense-data)))))
    (let ((avpt1 (average x1 x2))
          (avpt2 (average x2 x3)))
      (cons-stream (sign-change-detector avpt2 avpt1)
                   (make-zero-crossings (stream-cdr sense-data))))))

