
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.70.
;---
;It would be nice to be able to generate streams in which the pairs appear in some useful
;order, rather than in the order that results from an ad hoc interleaving process. We can
;use a technique similar to the 'merge' procedure of Exercise 3.56, if we define a way to
;say that one pair of integers is "less than" another. One way to do this is to define a
;"weighting function" W(i, j) and stipulate that (i1, j1) is less than (i2, j2) if
;W(i1, j1) < W (i2, j2). Write a procedure 'merge-weighted' that is like 'merge', except
;that 'merge-weighted' takes an additional argument 'weight', which is a procedure that
;computes the weight of a pair, and is used to determine the order in which elements
;should appear in the resulting merged stream. Using this, generalize pairs to a
;procedure 'weighted-pairs' that takes two streams, together with a procedure that
;computes a weighting function, and generates the stream of pairs, ordered according to
;'weight'. Use your procedure to generate
;---
;(a) the stream of all pairs of positive integers (i, j) with i <= j ordered according to
;    the sum i + j;
;---
;(b) the stream of all pairs of positive integers (i, j) with i <= j, where neither i nor
;    j is divisible by 2, 3, or 5, and the pairs are ordered according to the sum
;    2i + 3j + 5ij.
;------------------------------------------------------------------------------------------

;Original 'merge' procedure
;---
;Note that such a procedure asssumes that both input streams are constituted by integers.
;If the elements of the input streams are, instead, pairs, the problem becomes quite more
;convoluted. How to discern whether the combination of elements of a pair is smaller than
;the combination of elements of another pair? To do that, we have to have some kind of
;function that, given a pair [or, rather, its elements], attributes a weight according to
;the respective elements. That is to say that 'merge-weighted' either incorporates a
;default function computing these weights or the afore-mentioned function is provided to
;the procedure as an argument. The authors went for the latter option, so let's stick to
;it. For now, let's implement the original 'merge'. Afterwards, 'merge-weighted' is
;readily extrapolated from the original implementation.
;---
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car
                               (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car
                               (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

;'merge-weighted'
;---
;It is assumed that 'weight' is a function of two arguments, i.e., the unpacked elements
;of a given pair, rather than the pair itself. We could have went for a function of a
;single argument (i.e., the pair); nonetheless, let's hold on to the original idea.
;---
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (s1-i (car s1car)) (s1-j (cadr s1car))
                (s2-i (car s2car)) (s2-j (cadr s2car)))
           (cond ((<= (weight s1-i s1-j) (weight s2-i s2-j))
                  (cons-stream s1-car
                               (merge-weighted (stream-cdr s1)
                                               s2
                                               weight)))
                 (else
                  (cons-stream s2-car
                               (merge-weighted s1
                                               (stream-cdr s2)
                                               weight))))))))

;Implementing 'pairs' in terms of 'merge-weighted'
;---
;Note that 'pairs-weighted' also has an extra argument, namely 'weight'. Also note that,
;having defined 'pairs-weighted' in terms of 'merge-pairs', solving parts (a) and (b)
;becomes quite trivial.
;---
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr s))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;(a)
;---
(define (pairs-a s1 s2)
  (weighted-pairs s1 s2 (lambda (x y) (+ x y))))

;(b)
;---
(define (divisible-235? x)
  (or (= (remainder x 2) 0)
      (= (remainder x 3) 0)
      (= (remainder x 5) 0)))
;---
(define (pairs-b s1 s2)
  (stream-filter
   (lambda (p)
     (not (or (divisible-235? (car p))
              (divisible-235? (cadr p)))))
   (weighted-pairs s1
                   s2
                   (lambda (x y)
                     (+ (* 2 x) (* 3 y) (* 5 x y))))))

