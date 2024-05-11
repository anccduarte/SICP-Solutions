
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.67.
;---
;Modify the 'pairs' procedure so that (pairs integers integers) will produce the stream
;of all pairs of integers (i, j) (without the condition i<=j). Hint: You will need to mix
;in an additional stream.
;------------------------------------------------------------------------------------------

;Original implementation of 'pairs'
;---
;(S0 T0) (S0 T1) (S0 T2) ...  =>   (S0 T0) | (S0 T1) (S0 T2)
;(S1 T0) (S1 T1) (S1 T2) ...      ---------+-----------------
;(S2 T0) (S2 T1) (S2 T2) ...               | (S1 T1) (S1 T2)
;                                          |         (S2 T2)
;---
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (stream-car s) x)
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
;---
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;'all-pairs'
;---
;It also considers the pairs lying below the arrays's diagonal. That is to say, the pairs
;(i, j) such that i>j. This simply entails mixing in an additional stream, specifically:
;---
;(stream-map (lambda (x) (list x (stream-car t)))
;            (stream-cdr s))
;---
;The procedure may then be constructed by "consing" the "cars" of the streams onto the
;interleaving of this newly constructed stream and the streams interleaved in 'pairs'.
;---
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))               ;upper-left pair
   (interleave
    (stream-map (lambda (x) (list x (stream-car t)))  ;pairs lying in left column
                (stream-cdr s))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x)) ;pairs lying in top row
                 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t))))))    ;remaining pairs

