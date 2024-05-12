
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.69.
;---
;Write a procedure 'triples' that takes three infinite streams, 'S', 'T', and 'U', and
;produces the stream of triples (Si, Tj, Uk) such that i <= j <= k. Use 'triples' to
;generate the stream of all Pythagorean triples of positive integers, i.e., the triples
;(i, j, k) such that i <= j and i^2 + j^2 = k^2.
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;ANSWER TO EXERCISE
;------------------------------------------------------------------------------------------

;Preamble
;---
;To construct all triples (i, j, k), with i <= j <= k, we follow a similar strategy to
;one devised by the authors [see 3.66 for details]. However, we have to mix in an extra
;dimension. To do that, we simply have to add another stream to the blend. Considering
;the whole triples construct as a cubic array, we may regard the solution by pairs as a
;single horizontal slice of the cube [obviously keeping in mind that the pairs generated
;by such a solution lack the extra dimension provided by stream u]. The remaining slices
;are attained by maintaining streams s and t intact, and exploiting (stream-cdr u).

;'triples'
;---
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))     ;1st slice's top-left corner
   (interleave
    (stream-map (lambda (x)
                  (list x (stream-car t) (stream-car u)))  ;1st slices's 1st column
                (stream-cdr s))
    (interleave
     (stream-map (lambda (x)
                   (list (stream-car s) x (stream-car u))) ;1st slice's 1st row
                 (stream-cdr t))
     (interleave
      (triples (stream-cdr s) (stream-cdr t) u)            ;1st slice's remaining triples
      (triples s t (stream-cdr u)))))))                    ;all remaining slices

;'pythagorean-triples'
;---
(define pythagorean-triples
  (stream-filter
   (lambda (t) (= (+ (square (car t)) (square (cadr t)))
                  (square (caddr t))))
   (triples integers integers integers)))


;------------------------------------------------------------------------------------------
;EXTRA
;------------------------------------------------------------------------------------------

;For fun, here's a Pythagoreans triples implementation based on lists. Given an integer
;n, all Pythagorean triples (i, j, k), with k <= n are computed using this solution.
;[Note that all functional procedures except for 'map' have to be defined, since they are
;not native to Scheme.]
;---
(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (accumulate op init (cdr lst)))))
;---
(define (flatmap proc lst)
  (accumulate append '() (map proc lst)))
;---
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))
;---
(define (filter pred? lst)
  (cond ((null? lst) '())
        ((pred? (car lst))
         (cons (car lst) (filter pred? (cdr lst))))
        (else
         (filter pred? (cdr lst)))))
;---
(define (triples-up-to n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval j n)))
                      (enumerate-interval i n)))
           (enumerate-interval 1 n)))
;---
(define (pythagorean-triples-up-to n)
  (define square (lambda (n) (* n n)))
  (filter (lambda (t)
            (= (+ (square (car t)) (square (cadr t)))
               (square (caddr t))))
          (triples-up-to n)))
;---
(pythagorean-triples-up-to 6) ;((3 4 5))

