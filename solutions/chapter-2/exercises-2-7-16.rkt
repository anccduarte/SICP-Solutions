
#lang sicp


;------------------------------------------------------------------------------------------
;EXTENDED EXERCISE: INTERVAL ARITHMETIC (2.7. to 2.16.)
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;EXERCISE 2.7.
;---
;Alyssa's program  (SEE BELOW *) is incomplete because she has not specified the
;implementation of the interval abstraction. Here is a definition of the interval
;constructor (SEE BELOW **). Define selectors 'upper-bound' and 'lower-bound' to complete
;the implementation.
;------------------------------------------------------------------------------------------

;(*) addition of intervals
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
;---
;(*) multiplication of intervals -> assumes the intervals may include negative values
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
;---
;(*) division of intervals
;"Note that the bounds of the reciprocal interval are the reciprocal of the upper bound
;and the reciprocal of the lower bound, in that order."
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;(**) defining the constructor and selectors for intervals
;assumes that 'cons' takes a lower bound and an upper bound as arguments
;---
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))


;------------------------------------------------------------------------------------------
;EXERCISE 2.8.
;---
;Using reasoning analogous to Alyssa's, describe how the difference of two intervals may
;be computed. Define a corresponding subtraction procedure, called 'sub-interval'.
;------------------------------------------------------------------------------------------

;in subtracting two intervals A and B, the minimum value is attained by subtracting the
;maximum value of B (B's upper bound) from the minimum value of A (A's lower bound), and
;the maximum value results from subtracting the minimum value of B (B's lower bound)
;from the maximum value of A (A's upper bound). thus:
;A = [la, ua] and B = [lb, ub] => A-B = [la-ub, ua-lb]
;---
(define (sub-interval-v1 x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;'sub-interval' can also be defined in terms of the addition of intervals
;---
(define (sub-interval-v2 x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

;testing both implementations of 'sub-interval'
(display "EXERCISE 2.8.") (newline)
(let ((A (make-interval 2 8))
      (B (make-interval 1 3)))
  (display "A-B (v1) = ") (display (sub-interval-v1 A B)) (newline)
  (display "A-B (v2) = ") (display (sub-interval-v2 A B)) (newline))


;------------------------------------------------------------------------------------------
;EXERCISE 2.9.
;---
;The width of an interval is half of the difference between its upper and lower bounds.
;The width is a measure of the uncertainty of the number specified by the interval. For
;some arithmetic operations the width of the result of combining two intervals is a
;function only of the widths of the argument intervals, whereas for others the width of
;the combination is not a function of the widths of the argument intervals. Show that the
;width of the sum (or difference) of two intervals is a function only of the widths of
;the intervals being added (or subtracted). Give examples to show that this is not true
;for multiplication or division.
;------------------------------------------------------------------------------------------

;addition and subtraction -> no code, just math...
;---
;A = [la, ua]; B = [lb, ub]
;width(A) = (ua - la) / 2; width(B) = (ub - lb) / 2
;---
;addition
;A+B = [la+lb, ua+ub]
;width(A+B) = (ua + ub - la - lb) / 2 = (ua - la) / 2 + (ub - lb) / 2 =
;           = width(A) + width(B)
;---
;subtraction
;A-B = [la-ub, ua-lb]
;width(A-B) = (ua - lb - la + ub) / 2 = (ua - la) / 2 + (ub - lb) / 2 =
;           = width(A) + width(B)

;multiplication and division -> examples
;if the width of the product/division of two intervals is a function of the width of the
;intervals which comprise the arithmetic operation, then multiplying/dividing intervals
;of arbitrary widths w1 and w2 should always produce an interval having a fixed width
;independent of the lower and upper bounds of the operands
;---
;testing multiplication of random intervals with same widths
;same reasoning can be applied to division
(display "EXERCISE 2.9.") (newline)
(define (print-widths a b)
  (define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2.0))
  (display "width(A) = ") (display (width a)) (display " | ")
  (display "width(B) = ") (display (width b)) (display " | ")
  (let ((c (mul-interval a b)))
    (display "width(C) = ") (display (width c)) (newline)))
;---
;case 1
(let ((A (make-interval 1 2))
      (B (make-interval 3 4)))
  (print-widths A B))
;---
;case 2
(let ((A (make-interval 5 6))
      (B (make-interval 7 8)))
  (print-widths A B))


;------------------------------------------------------------------------------------------
;EXERCISE 2.10.
;---
;Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments
;that it is not clear what it means to divide by an interval that spans zero. Modify
;Alyssa’s code to check for this condition and to signal an error if it occurs.
;------------------------------------------------------------------------------------------

;if the interval spans zero, throw an error
;otherwise, call the original procedure ('div-interval')
;---
(define (positive? x) (> x 0))
(define (div-interval-new a b)
  (if (positive? (* (lower-bound b) (upper-bound b)))
      (div-interval a b)
      (error "Division by zero: divisor spans 0 ->" b)))


;------------------------------------------------------------------------------------------
;EXERCISE 2.11.
;---
;In passing, Ben also cryptically comments: "By testing the signs of the endpoints of the
;intervals, it is possible to break 'mul-interval' into nine cases, only one of which
;requires more than two multiplications." Rewrite this procedure using Ben's suggestion.
;------------------------------------------------------------------------------------------

;"reassign" symbols
;">" -> larger than or equal to
;"<" -> smaller than
;---
(define (neg? x) (< x 0))
(define (pos? x) (not (neg? x)))

;intervals: [a,b], [c,d]
;allowed [a,b] intervals: [a<0,b<0], [a<0,b>0], [a>0,b>0]
;allowed [c,d] intervals: [c<0,d<0], [c<0,d>0], [c>0,d>0]
;---
;break the multiplication problem into nine different cases
;1st: if [a<0,b<0] and [c<0,d<0] => [b*d, a*c]
;2nd: if [a<0,b<0] and [c<0,d>0] => [a*d, a*c]
;3rd: if [a<0,b<0] and [c>0,d>0] => [a*d, b*c]
;4th: if [a<0,b>0] and [c<0,d<0] => [b*c, a*c]
;5th; if [a<0,b>0] and [c<0,d>0] => [min(a*d, b*c), max(a*c, b*d)]
;6th: if [a<0,b>0] and [c>0,d>0] => [a*d, b*d]
;7th: if [a>0,b>0] and [c<0,d<0] => [b*c, a*d]
;8th; if [a>0,b>0] and [c<0,d>0] => [b*c, b*d]
;9th: if [a>0,b>0] and [c>0,d>0] => [a*c, b*d]

;actual implementation -> there might be errors...
;---
(define (mul-interval-v2 x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (neg? a) (neg? b) (neg? c) (neg? d))
           (make-interval (* b d) (* a c)))
          ((and (neg? a) (neg? b) (neg? c) (pos? d))
           (make-interval (* a d) (* a c)))
          ((and (neg? a) (neg? b) (pos? c) (pos? d))
           (make-interval (* a d) (* b c)))
          ((and (neg? a) (pos? b) (neg? c) (neg? d))
           (make-interval (* b c) (* a c)))
          ((and (neg? a) (pos? b) (neg? c) (pos? d))
           (make-interval (min (* a d) (* b c))
                          (max (* a c) (* b d))))
          ((and (neg? a) (pos? b) (pos? c) (pos? d))
           (make-interval (* a d) (* b d)))
          ((and (pos? a) (pos? b) (neg? c) (neg? d))
           (make-interval (* b c) (* a d)))
          ((and (pos? a) (pos? b) (neg? c) (pos? d))
           (make-interval (* b c) (* b d)))
          ((and (pos? a) (pos? b) (pos? c) (pos? d))
           (make-interval (* a c) (* b d))))))

;test both implementations of 'mul-interval' (covering the 9 cases)
;---
(display "EXERCISE 2.11.") (newline)
(define (display-interval i)
  (display "[") (display (lower-bound i)) (display ",")
  (display (upper-bound i)) (display "]"))
;---
(define (test-mul a b)
  (let ((m1 (mul-interval a b))
        (m2 (mul-interval-v2 a b)))
    (display "(mul-interval ") (display-interval a) (display " ")
    (display-interval b) (display ") -> ") (display m1) (newline)
    (display "(mul-interval-v2 ") (display-interval a) (display " ")
    (display-interval b) (display ") -> ") (display m2) (newline)))
;---
;1st case
(let ((A (make-interval -2 -1))
      (B (make-interval -2 -1)))
  (test-mul A B))
;---
;2nd case
(let ((A (make-interval -2 -1))
      (B (make-interval -2 1)))
  (test-mul A B))
;---
;3rd case
(let ((A (make-interval -2 -1))
      (B (make-interval 1 2)))
  (test-mul A B))
;---
;4th case
(let ((A (make-interval -2 1))
      (B (make-interval -2 -1)))
  (test-mul A B))
;---
;5th case
(let ((A (make-interval -2 1))
      (B (make-interval -2 1)))
  (test-mul A B))
;---
;6th case
(let ((A (make-interval -2 1))
      (B (make-interval 1 2)))
  (test-mul A B))
;---
;7th case
(let ((A (make-interval 1 2))
      (B (make-interval -2 -1)))
  (test-mul A B))
;---
;8th case
(let ((A (make-interval 1 2))
      (B (make-interval -2 1)))
  (test-mul A B))
;---
;9th case
(let ((A (make-interval 1 2))
      (B (make-interval 1 2)))
  (test-mul A B))


;------------------------------------------------------------------------------------------
;EXERCISE 2.12.
;---
;After debugging her program, Alyssa shows it to a potential user, who complains that her
;program solves the wrong problem. He wants a program that can deal with numbers
;represented as a center value and an additive tolerance; for example, he wants to work
;with intervals such as (3.5 +/- 0.15) rather than [3.35, 3.65]. Alyssa returns to her
;desk and fixes this problem by supplying an alternate constructor and alternate
;selectors (SEE BELOW ***). Unfortunately, most of Alyss's users are engineers. Real
;engineering situations usually involve measurements with only a small uncertainty,
;measured as the ratio of the width of the interval to the midpoint of the interval.
;Engineers usually specify percentage tolerances on the parameters of devices, as in the
;resistor specifications given earlier [6.8 ohms with 10% tolerance is equivalent to
;[6.8 - 0.68, 6.8 + 0.68] = [6.12, 7.48] in interval notation]. Define a constructor
;'make-center-percent' that takes a center and a percentage tolerance and produces the
;desired interval. You must also define a selector 'percent' that produces the percentage
;tolerance for a given interval. The 'center' selector is the same as the one shown.
;------------------------------------------------------------------------------------------

;(***) provided in text
;---
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) / 2.0))

;the actual constructor and selectors
;---
;constructor -> 'make-center-percent'
;percentage tolerance = width / center => width = percentage tolerance * center
;if 'center' is a negative number, then 'width' would be negative, which would not make
;sense. hence, widht = abs(percentage tolerance * center)
(define (make-center-percent c p)
  (let ((w (abs (* c p))))
    (make-interval (- c w) (+ c w))))
;---
;selectors -> 'center' (already implemented) and 'percent'
;percent-tol(A) = width(A) / center(A) = (upper-bound(A) - center(A)) / center(A)
(define (percent i)
  (/ (- (upper-bound i) (center i))
     (center i)))

;test constructor and selectors
;---
(display "EXERCISE 2.12.") (newline)
(let ((A (make-center-percent 6.8 0.1)))
  (display "A = ") (display-interval A) (newline)
  (display "center(A) = ") (display (center A)) (newline)
  (display "percent-tol(A) = ") (display (percent A)) (newline))


;------------------------------------------------------------------------------------------
;EXERCISE 2.13.
;---
;Show that under the assumption of small percentage tolerances there is a simple formula
;for the approximate percentage tolerance of the product of two intervals in terms of the
;tolerances of the factors. You may simplify the problem by assuming that all numbers are
;positive.
;------------------------------------------------------------------------------------------

;no code, just math...
;---
;assume A = [la, ua] and B = [lb, ub], with la>0, lb>0, ua>0 and ub>0
;then, the product of the two intervals is given by A*B = [la*lb, ua*ub]
;the intervals A and B may also be represented in terms of percentage tolerances:
;A = [ca - ca*pa, ca + ca*pa] and B = [cb - cb*pb, cb +cb*pb]
;hence, A*B = [la*lb, ua*ub] = [(ca - ca*pa)*(cb - cb*pb), (ca + ca*pa)*(cb + cb*pb)] =
;           = [ca*cb - ca*cb*pb - ca*cb*pa + ca*cb*pa*pb,
;              ca*cb + ca*cb*pb + ca*cb*pa + ca*cb*pa*pb]
;the center of an interval I is defined as (lower-bound(I) + upper-bound(I)) / 2
;so, the center of the interval A*B is:
;center(A*B) = (ca*cb - ca*cb*pb - ca*cb*pa + ca*cb*pa*pb +
;              + ca*cb + ca*cb*pb + ca*cb*pa + ca*cb*pa*pb) / 2 =
;            = (2*ca*cb + 2*ca*cb*pa*pb) / 2 = ca*cb + ca*cb*pa*pb
;the percentage tolerance of an interval I may be defined in terms of the center of I as
;(upper-bound(I) - center(I)) / center(I)
;therefore, the percentage tolerance of the interval A*B is:
;percent-tol(A*B) = (ca*cb + ca*cb*pb + ca*cb*pa + ca*cb*pa*pb - ca*cb - ca*cb*pa*pb) /
;                   / (ca*cb + ca*cb*pa*pb) =
;                 = (ca*cb*pa + ca*cb*pb) / (ca*cb + ca*cb*pa*pb) =
;                 = (ca*cb*(pa+pb)) / (ca*cb*(1+pa*pb)) = (pa+pb) / (1+pa*pb)
;assuming that pa and pb are small, the term pa*pb may be ignored, resulting in:
;percent-tol(A*B) = pa+pb = percent-tol(A) + percent-tol(B)

;testing for some random intervals (with small percentage tolerances)
;---
(display "EXERCISE 2.13.") (newline)
(let ((A (make-center-percent 10 0.05))
      (B (make-center-percent 4 0.06)))
  (let ((C (mul-interval A B)))
    (let ((perc-a (percent A))
          (perc-b (percent B))
          (perc-c (percent C)))
      (display "percent-tol(C) (actual) = ") (display perc-c) (newline)
      (display "percent-tol(C) (estimated) = ")
      (display (+ perc-a perc-b)) (newline)
      (let ((abs-diff (abs (- (+ perc-a perc-b) perc-c))))
        (display "abs-diff(actual, estimated) = ") (display abs-diff) (newline)))))


;------------------------------------------------------------------------------------------
;EXERCISE 2.14.
;---
;Demonstrate that Lem is right (SEE BELOW ****). Investigate the behavior of the system
;on a variety of arithmetic expressions. Make some intervals A and B, and use them in
;computing the expressions A/A and A/B. You will get the most insight by using intervals
;whose width is a small percentage of the center value. Examine the results of the
;computation in center-percent form (see Exercise 2.12).
;------------------------------------------------------------------------------------------

;(****) distinct implementations of the parallel-resistors formula -> although
;algebraically equivalent, produce distinct results
;---
;1st formula -> (r1*r2) / (r1+r2)
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
;---
;2nd formula -> 1 / (1/r1 + 1/r2)
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;very tough
;refer to http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16


;------------------------------------------------------------------------------------------
;EXERCISE 2.15.
;---
;Eva Lu Ator, another user, has also noticed the different intervals computed by
;different but algebraically equivalent expressions. She says that a formula to compute
;with intervals using Alyssa's system will produce tighter error bounds if it can be
;written in such a form that no variable that represents an uncertain number is repeated.
;Thus, she says, 'par2' is a "better" program for parallel resistances than 'par1'. Is
;she right? Why?
;------------------------------------------------------------------------------------------

;very tough
;refer to http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16


;------------------------------------------------------------------------------------------
;EXERCISE 2.16.
;---
;Explain, in general, why equivalent algebraic expressions may lead to different answers.
;Can you devise an interval-arithmetic package that does not have this shortcoming, or is
;this task impossible? (Warning: This problem is very difficult.)
;------------------------------------------------------------------------------------------

;very tough
;refer to http://community.schemewiki.org/?sicp-ex-2.14-2.15-2.16

