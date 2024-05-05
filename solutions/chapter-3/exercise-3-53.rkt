
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.53.
;---
;Without running the program, describe the elements of the stream defined by
;---
;(define s (cons-stream 1 (add-streams s s)))
;------------------------------------------------------------------------------------------

;computing the elements of 's'
;---
;the definition says that 's' is a stream starting at 1, such that the remaining of the
;stream may be computed by adding 's' to itself; we promptly identify the definition to
;generate the stream comprised of the powers of 2:
;---
;   1  2  4   8  16  32   64  128  ...  =  s
;   1  2  4   8  16  32   64  128  ...  =  s
;1  2  4  8  16  32  64  128  256  ...  =  s
;---
;the 1st element of 's' is added to itself to generate the 2nd element of the stream; the
;2nd element of 's' is added to itself to produce the 3rd element of the stream; and so
;on and so forth

