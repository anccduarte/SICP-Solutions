
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.75.
;---
;Implement the constructor 'make-from-mag-ang' in message-passing style. This procedure
;should be analogous to the 'make-from-real-imag' procedure given above (SEE BELOW *).
;------------------------------------------------------------------------------------------

;"This style of programming is called message passing. The name comes from the image that
;a data object is an entity that receives the requested operation name as a 'message'. We
;have already seen an example of message passing in Section 2.1.3, where we saw how
;'cons', 'car', and 'cdr' could be defined with no data objects but only procedures. Here
;we see that message passing is not a mathematical trick but a useful technique for
;organizing systems with generic operations. In the remainder of this chapter we will
;continue to use data-directed programming, rather than message passing, to discuss
;generic arithmetic operations. In Chapter 3 we will return to message passing, and we
;will see that it can be a powerful tool for structuring simulation programs."

;(*) 'make-rectangular' (provided in the text)
;---
(define (make-rectangular x y)
  (define (square x) (* x x))
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "unknown operation:" op))))
  dispatch)

;'make-polar'
;---
(define (make-polar m a)
  (lambda (op)
    (cond ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          (else (error "unknown operation:" op)))))

;'apply-generic'
;---
(define (apply-generic op arg)
  (arg op))

;test for random complex numbers
;---
(define (test z)
  (let ((lst '(real-part imag-part magnitude angle)))
    (map (lambda (op) (apply-generic op z))
         lst)))
;---
(define zr (make-rectangular 1 2))
(define zp (make-polar 3 4))
;---
(test zr)
(test zp)

