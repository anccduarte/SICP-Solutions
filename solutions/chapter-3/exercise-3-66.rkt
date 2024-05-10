
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.66.
;---
;Examine the stream (pairs integers integers). Can you make any general comments about
;the order in which the pairs are placed into the stream? For example, approximately how
;many pairs precede the pair (1, 100)? the pair (99, 100)? the pair (100, 100)? (If you
;can make precise mathematical statements here, all the better. But feel free to give
;more qualitative answers if you find yourself getting bogged down.)
;------------------------------------------------------------------------------------------

;'integers'
;---
(define integers
  (cons-stream 1 (add-streams ones integers)))
;---
(define ones
  (cons-stream 1 ones))

;'pairs'
;---
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (car-stream t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
;---
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;Representation of pairs [before and after "filtering"]
;---
;(S0 T0) (S0 T1) (S0 T2) ...  =>  1 (S0 T0) | (S0 T1) (S0 T2) ... 2
;(S1 T0) (S1 T1) (S1 T2) ...        --------+---------------------
;(S2 T0) (S2 T1) (S2 T2) ...                | (S1 T1) (S1 T2) ... 3
;                                           |         (S2 T2) ...
;---
;1. (list (stream-car s) (stream-car t))
;2. (stream-map (lambda (x) (list (stream-car s) x))
;               (stream-cdr t))
;3. (pairs (stream-cdr s) (stream-cdr t))

;Answer
;[note that the solution is empirical, i.e., based on extrapolations made based on
;observed patterns; no formal mathematical proof is provided]
;---
;Below, we depict the array of integer pairs (i j), such that j>=i. The method by which
;these pairs are severed is identical to the previous scheme. The listing of pairs is
;generated via executing (pairs integers integers), i.e., both formal parameters s and t
;are assigned to 'integers'.
;---
;(S0 T0) (S0 T1) (S0 T2) ...  =>  (1 1) (1 2) (1 3) ...
;        (S1 T1) (S1 T2) ...            (2 2) (2 3) ...
;                (S2 T2) ...                  (3 3) ...
;---
;The first pair to be selected is (1 1). After that, pairs (1 n), with n>1, are chosen in
;2nd, 4th, 6th, ..., ath (a%2=0) places [note that these are interleaved with the pairs
;resulting from (pairs (stream-cdr s) (stream-cdr t))]. Then, the 3rd choice is the
;result of (stream-car (pairs (stream-cdr s) (stream-cdr t))), that is, the pair (2 2).
;Now, the 5th, 9th, 13th, etc. picks will be pairs starting with 2. Next, the 7th term is
;given by (stream-car (pairs (stream-cdr (stream-cdr s)) (stream-cdr (stream-cdr t)))),
;which is to say (3 3). Afterwards, the 11th, 19th, 27th, etc. places are taken by pairs
;starting with 3, that is, pairs (3 n), n>3. The reasoning follows similarly for the
;remaining pairs.
;---
;We may now divide the pairs into two subclasses: pairs (x y) such that x=y and pairs
;(x y) such that y>x. The pattern is quite easily uncovered in the latter case. Here, we
;have that the pairs (1 1), (2 2) and (3 3) are chosen in 1st, 3rd and 7th places,
;respectively. This hints that a pair (x y), with x=y, is selected in (2^n - 1)th place.
;Computing the places at which elements the former subcategory of pairs are picked is
;more convoluted. Note the following pattern that is readily generalized below:
;---
;1st (2 n), n>2 at place 5  -> 2^2 + 1
;2nd (2 n), n>2 at place 9  -> 2^2 + 1 + 2^2    = 2(2^2) + 1
;3rd (2 n), n>2 at place 13 -> 2^2 + 1 + 2(2^2) = 3(2^2) + 1
;and so on...
;---
;1st (3 n), n>3 at place 11 -> 2^3 + 3
;2nd (3 n), n>3 at place 19 -> 2^3 + 3 + 2^3    = 2(2^3) + 3
;3rd (3 n), n>3 at place 27 -> 2^3 + 3 + 2(2^3) = 3(2^3) + 3
;and so on...
;---
;1st (4 n), n>4 at place 21 -> 2^4 + 5
;2nd (4 n), n>4 at place 37 -> 2^4 + 5 + 2^4    = 2(2^4) + 5
;3rd (4 n), n>4 at place 53 -> 2^4 + 3 + 2(2^4) = 3(2^4) + 5
;and so on...
;---
;1st (x y), y>x at place 2^x + 2(x-1) - 1
;2nd (x y), y>x at place 2^x + 2(x-1) - 1 + 2^x    = 2(2^x) + 2(x-1) - 1
;3rd (x y), y>x at place 2^x + 2(x-1) - 1 + 2(2^x) = 3(2^x) + 2(x-1) - 1
;...
;nth (x y), y>x at place n(2^x) + 2(x-1) - 1
;[note that n is simply y-x, hence the nth element lies at (y-x)(2^x) + 2(x-1) - 1]
;---
;We now have a "function f of two arguments such that the pair corresponding to element i
;of the first stream and element j of the second stream will appear as element number
;f(i, j) of the output stream"! This function is given by:
;---
;f(i, j) = 2^i - 1,                 if i=j
;        = (j-i)(2^i) + 2(i-1) - 1, if j>i

;Approximately how many pairs precede the pair (1, 100)? the pair (99, 100)? the pair
;(100, 100)?
;---
;f(1, 100)   = (100-1)(2^1) + 2(1-1) - 1 = 99*2 + 2*0 - 1 = 197
;f(99, 100)  = (100-99)(2^99) + 2(99-1) - 1 = 2^99 + 196 - 1 = 6.338253e+29
;f(100, 100) = 2^100 - 1 = 1.267651e+30

