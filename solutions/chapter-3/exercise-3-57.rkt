
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.57.
;---
;How many additions are performed when we compute the nth Fibonacci number using the
;definition of 'fibs' based on the 'add-streams' procedure? Show that the number of
;additions would be exponentially greater if we had implemented (delay <exp>) simply as
;(lambda() <exp>), without using the optimization provided by the 'memo-proc' procedure
;described in Section 3.5.1.
;------------------------------------------------------------------------------------------

;Footnote 64
;---
;"This exercise shows how call-by-need is closely related to ordinary memoization as
;described in Exercise 3.27. In that exercise, we used assignment to explicitly construct
;a local table. Our call-by-need stream optimization effectively constructs such a table
;automatically, storing values in the previously forced parts of the stream."

;Defining 'fibs' in terms of 'add-streams'
;---
(define fibs (cons-stream 0
                          (cons-stream 1
                                       (add-streams (stream-cdr fibs)
                                                    fibs))))
;---
;      1  1  2  3  5   8  13  21  34  55  ...  =  (stream-cdr fibs)
;      0  1  1  2  3   5   8  13  21  34  ...  =  fibs
;0  1  1  2  3  5  8  13  21  34  55  89  ...  =  fibs

;Answer
;---
;As we move down the stream of Fibonacci numbers, an addition is executed ('add-streams')
;and the result is memoized. This is the case since the delayed object is wrapped by
;'memo-proc', which avoids recalculation. Hence, computing the nth Fibonacci requires n
;additions.
;---
;This does not hold if 'delay' were simply implemented as (lambda () <exp>). In such a
;circumstance, every call to 'stream-cdr' would result in the addition of two un-memoized
;calls to 'fibs' (one in the form 'fibs' and the other in the form (stream-cdr fibs)).
;Both these calls execute themselves the addition of two un-memoized calls to 'fibs', and
;so on and so forth. It is clear that a tree-like structure similar to the one analyzed
;in previous chapters (illustrating the evolution of recursive Fibonacci processes) comes
;to light. In this context, to compute the nth Fibonacci number, we ought to execute a
;number of additions proportional to 2^n.

