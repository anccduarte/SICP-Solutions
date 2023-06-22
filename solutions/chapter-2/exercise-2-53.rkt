
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.53.
;---
;What would the interpreter print in response to evaluating each of the following
;expressions? (SEE BELOW)
;------------------------------------------------------------------------------------------

;'memq'
;---
(define (memq item x)
  (cond ((null? x) false)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

;1.
;---
(list 'a 'b 'c)
;(a b c)

;2.
;---
(list (list 'george))
;((george))

;3.
;---
(cdr '((x1 x2) (y1 y2)))
;((y1 y2))

;4.
;---
(cadr '((x1 x2) (y1 y2)))
;(y1 y2)

;5.
;---
(pair? (car '(a short list)))
;(pair? 'a) -> #f

;6.
;---
(memq 'red '((red shoes) (blue socks)))
;#f

;7.
;---
(memq 'red '(red shoes blue socks))
;(red shoes blue socks)

