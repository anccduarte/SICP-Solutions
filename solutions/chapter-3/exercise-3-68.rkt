
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.68.
;---
;Louis Reasoner thinks that building a stream of pairs from three parts is unnecessarily
;complicated. Instead of separating the pair (S0, T0) from the rest of the pairs in the
;first row, he proposes to work with the whole first row, as follows [see below *]. Does
;this work? Consider what happens if we evaluate (pairs integers integers) using Louis's
;definition of pairs. 
;------------------------------------------------------------------------------------------

;(*) Reasoner's suggestion
;---
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

;Answer
;---
;Reasoner's suggestion seems fine. At first glance, we may devise a function f(x, y) such
;that f(x, y) corresponds to the position at which the pair (x, y) is picked by 'pairs'.
;It goes as follows [below, the pattern generalization is provided].
;---
;- The odd picks (i.e., 1st, 3rd, 5th, etc.) are pairs whose first element is 1, that is,
;  pairs (1, n), such that n>=1;
;- Pairs (2, n), n>=2, will be picked in places 2, 6, 10, 14, etc.;
;- Pairs (3, n), n>=3, are selected in places 4, 12, 20, 28, etc.; and so on...
;---
;Generalizing, pairs (x, y) are picked at positions 2^(x-1) + (y-x)2^x, that is, the
;above-mentioned function is given by:
;---
;f(x, y) = 2^(x-1) + (y-x)2^x
;---
;It all looks ok so far. However, a pernicious effect of discarding the first part of the
;stream built by the original version of 'pairs' is left unnoticed. Since 'interleave' is
;not a special form, it evaluates both its arguments when called. This defeats the
;purpose of delayed evaluation and streams, that is, 'pairs', instead of returning a
;stream object consisting of an element and a promise to compute the remaining elements,
;it infinitely loops through the streams of integers without ever returning. As clearly
;stated by xdavidliu [http://community.schemewiki.org/?sicp-ex-3.68]: "Louis'
;implementation will recurse infinitely simply because 'interleave' is an ordinary
;function, not a special form like 'cons-stream', and hence will need to fully evaluate
;both arguments first, since Scheme uses eager evaluation for ordinary functions. Since
;the second argument to 'interleave' is a recursive call to 'pairs', and there is no
;hard-coded base case, this implementation will recurse infinitely."

