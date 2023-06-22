
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.55.
;---
;Eva Lu Ator types to the interpreter the expression
;---
;(car ''abracadabra)
;---
;To her surprise, the interpreter prints back 'quote'. Explain.
;------------------------------------------------------------------------------------------

;the expression (car ''abracadabra) evaluates to (car '(quote abracadabra)). since we
;want the 'car' of the symbolic expression '(quote abracadabra), it is ought to simply
;return 'quote'
;---
(car ''abracadabra)
(car '(quote abracadabra))

