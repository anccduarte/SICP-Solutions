
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.27.
;---
;Memoization (also called tabulation) is a technique that enables a procedure to record,
;in a local table, values that have previously been computed. This technique can make a
;vast difference in the performance of a program. A memoized procedure maintains a table
;in which values of previous calls are stored using as keys the arguments that produced
;the values. When the memoized procedure is asked to compute a value, it first checks the
;table to see if the value is already there and, if so, just returns that value.
;Otherwise, it computes the new value in the ordinary way and stores this in the table.
;As an example of memoization, recall from Section 1.2.2 the exponential process for
;computing Fibonacci numbers:
;---
;(define (fib n)
;  (cond ((= n 0) 0)
;        ((= n 1) 1)
;        (else (+ (fib (- n 1)) (fib (- n 2))))))
;---
;The memoized version of the same procedure is
;---
;(define memo-fib
;  (memoize
;   (lambda (n)
;     (cond ((= n 0) 0)
;           ((= n 1) 1)
;           (else (+ (memo-fib (- n 1))
;                    (memo-fib (- n 2))))))))
;---
;where the memoizer is defined as
;---
;(define (memoize f)
;  (let ((table (make-table)))
;    (lambda (x)
;      (let ((previously-computed-result
;             (lookup x table)))
;        (or previously-computed-result
;            (let ((result (f x)))
;              (insert! x result table)
;              result))))))
;---
;Draw an environment diagram to analyze the computation of '(memo-fib 3)'. Explain why
;'memo-fib' computes the 'nth' Fibonacci number in a number of steps proportional to 'n'.
;Would the scheme still work if we had simply defined 'memo-fib' to be '(memoize fib)'?
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;DEFINING ONE-DIMENSIONAL TABLES
;---
;Simplified implementation of tables: for tabulating Fibonacci there is no need for
;multidimensional tables.
;------------------------------------------------------------------------------------------

;one-dimensional tables
;---
(define (make-table) (list '*table*))
;---
(define (assoc key records)
  (cond ((null? records) #f)
        ((= key (caar records)) (car records))
        (else (assoc key (cdr records)))))
;---
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))
;---
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value)
                              (cdr table))))))


;------------------------------------------------------------------------------------------
;DEFINING 'MEMOIZE'
;---
;The procedure is tracked so that the memoization process may easily be visualized.
;------------------------------------------------------------------------------------------

;'track'
;---
(define (track x result)
  (display x)
  (display " -> ")
  (display result)
  (newline))

;'memoize'
;---
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((memoized-result (lookup x table)))
        (track x memoized-result)
        (or memoized-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))


;------------------------------------------------------------------------------------------
;DEFINING AND VISUALIZING 'MEMO-FIB'
;---
;'memo-fib', contrary to 'make-fib-2' [see explanation below] makes full use of the
;potentialities of tabulation. Whenever intermediate results are computed they are
;immediately stored at the table promptly created upon evaluation of 'memo-fib' by the
;Lisp interpreter. This means that 'memo-fib' is evaluated a single time for each value
;smaller than or equal than the argument it initially takes as input. Moreover, an nth
;(n > 1) call of 'memo-fib' makes use of all results stored at previous calls. That is,
;calling Fib(40) having called Fib(39) simply entails adding the results stored at the
;table for arguments 38 and 39. Below, a pictoreal representation of the evaluation of
;Fib(3) is provided.
;------------------------------------------------------------------------------------------

;'memo-fib'
;---
(define memo-fib
  (memoize
   (lambda (n)
     (if (< n 2)
         n
         (+ (memo-fib (- n 1))
            (memo-fib (- n 2)))))))

;evaluating '(memo-fib 3)'
;---
;                     +----------------------+
;                     ↓                      |
;             +--------------+        +---+-|-+
; global ---> | memoize ------------> | | | | |
; env         | memo-fib     |        +-|-+---+
;             +----|---------+          |
;                  |     ↑              +---> parameters: f
;   +--------------+     |                    body: (let ((table (...))) (...))
;   |                    |
;   |               +----------+      +---+---+     +---+---+     +---+---+     +---+---+     +---+---+
;   |               | table --------> | | | ------> | | | ------> | | | ------> | | | ------> | | | / |
;   |               +----------+      +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;   ↓                    ↑   ↑          ↓             ↓             ↓             ↓             ↓
; +---+---+              |   |       *table*        +---+---+     +---+---+     +---+---+     +---+---+
; | | | -----------------+   |                      | | | | |     | | | | |     | | | | |     | | | | |
; +-|-+---+                  |                      +-|-+-|-+     +-|-+-|-+     +-|-+-|-+     +-|-+-|-+
;   ↓                        |    +--------+          ↓   ↓         ↓   ↓         ↓   ↓         ↓   ↓
; parameters: x              +--- | x: 3   |          3   2         2   1         1   1         0   0
; body: (let ((memo-result   |    +--------+
;              (...)))       |         
;         (...))             |    +--------+     +--------+
;                            +--- | x: 2   | --- | x: 1   |
;                            |    +--------+     +--------+
;                            | 
;                            |    +--------+
;                            +--- | x: 1   |     [calls to 'memo-fib']
;                            |    +--------+
;                            | 
;                            |    +--------+
;                            +--- | x: 0   |
;                                 +--------+

;test 'memo-fib'
;---
(let ()
  (memo-fib 3)
  (display "---") (newline)
  (memo-fib 3)
  (display "---") (newline)
  (memo-fib 4)
  (display "---") (newline)
  (memo-fib 4)
  (newline))


;------------------------------------------------------------------------------------------
;DEFINING AND VISUALIZING 'MEMO-FIB-2'
;---
;At first glance, 'memo-fib-2' seems to perform just like 'memo-fib'. A more careful
;look at the process generated via calling 'memo-fib-2' reveals some complications,
;unveiling a basal fault that defeats the purpose of memoization in the first place: in
;the body of 'memoize', 'fib' is called instead of 'memo-fib-2'. This means that, when
;generating a result, the table is solely consulted for the argument 'memo-fib-2' takes
;as input and the only result it putatively stores at the table is the one corresponding
;to this same argument. Two major complications arise if the argument isn't already
;stored at the table: 1. the process still evolves in O(2^n) steps; 2. no intermediate
;results are saved. The former difficulty makes it unfeasible to compute the Fibonacci
;number associated to large arguments. The latter impediment also severely restrains the
;tabulation process: if we strenuously compute Fib(40) and then try to compute Fib(39),
;we may not utilize intermediate results in such a calculation. For such reasons,
;defining 'memo-fib' to simply be '(memoize fib)', is plainly purposeless. Below, a
;schematic representation of the evaluation of Fib(3) is provided.
;------------------------------------------------------------------------------------------

;'memo-fib-2'
;---
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
;---
(define memo-fib-2 (memoize fib))

;evaluating '(memo-fib-2 3)'
;---
;                     +-----------------------+
;                     ↓                       |
;             +----------------+              |
; global ---> | memoize (...)  |        +---+-|-+
; env         | fib ------------------> | | | | |
;             | memo-fib-2     |        +-|-+---+
;             +----|-----------+          |
;                  |     ↑  ↑             +---> parameters: n
;                  |     |  |                   body: (if (...) (...))
;   +--------------+     |  |
;   |                    |  |
;   |               +------------+      +---+---+     +---+---+
;   |               | table -------->   | | | ------> | | | / |
;   |               +------------+      +-|-+---+     +-|-+---+
;   ↓                    ↑  |  ↑          ↓             ↓
; +---+---+              |  |  |       *table*        +---+---+
; | | | -----------------+  |  |                      | | | | |
; +-|-+---+                 |  |                      +-|-+-|-+
;   ↓                       |  |    +--------+          ↓   ↓
; parameters: x             |  +--- | x: 3   | (*)      3   2
; body: (let ((memo-result  |       +--------+
;              (...)))      |         
;         (...))            |       +--------+     +--------+
;                           +------ | x: 2   | --- | x: 1   | (**)
;                           |       +--------+     +--------+
;                           | 
;                           |       +--------+
;                           +------ | x: 1   | (**)
;                           |       +--------+         [(*) call to 'memo-fib-2'
;                           |                          (**) calls to 'fib']
;                           |       +--------+
;                           +------ | x: 0   | (**)
;                                   +--------+

;test 'memo-fib-2'
;---
(let ()
  (memo-fib-2 4)
  (display "---") (newline)
  (memo-fib-2 4)
  (display "---") (newline)
  (memo-fib-2 3)
  (display "---") (newline)
  (memo-fib-2 3)
  (display ""))

