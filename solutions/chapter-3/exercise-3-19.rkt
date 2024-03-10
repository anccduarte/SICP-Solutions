
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.19.
;---
;Redo Exercise 3.18 using an algorithm that takes only a constant amount of space. (This
;requires a very clever idea.)
;------------------------------------------------------------------------------------------

;'contains-cycle?'
;(the exact same solution to Exercise 3.18 is provided here; such an implements requires
;a constant amount of space and time, since, in the worst case scenario, the list of
;visited pairs 'visited' is to contain at maximum the number of pairs included in the
;list passed as argument to 'contains-cycle?'; for an alternative solution implemeting
;Floyd's algorithm refer to "community.schemewiki.org/?sicp-ex-3.19")
;---
(define (contains-cycle? x)
  ;---
  (define (iter p visited)
    (cond ((not (pair? p)) #f) ;'() isn't a pair
          ((memq p visited) #t)
          (else
           (or (iter (car p) (cons p visited))
               (iter (cdr p) (cons p visited))))))
  ;---
  (iter x '()))

