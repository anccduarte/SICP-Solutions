
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.14.
;---
;The following procedure is quite useful, although obscure:
;---
;(define (mystery x)
;  (define (loop x y)
;    (if (null? x)
;        y
;        (let ((temp (cdr x)))
;          (set-cdr! x y)
;          (loop temp x))))
;  (loop x '()))
;---
;'loop' uses the 'temporary' variable 'temp' to hold the old value of the 'cdr' of 'x',
;since the 'set-cdr!' on the next line destroys the 'cdr'. Explain what 'mystery' does in
;general. Suppose 'v' is defined by (define v (list 'a 'b 'c 'd)). Draw the
;box-and-pointer diagram that represents the list to which 'v' is bound. Suppose that we
;now evaluate (define w (mystery v)). Draw box-and-pointer diagrams that show the
;structures 'v' and 'w' after evaluating this expression. What would be printed as the
;values of 'v' and 'w'?
;------------------------------------------------------------------------------------------

;defining 'mystery'
;---
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;defining and representing 'v'
;---
(define v '(a b c d)) v
;---
;        +---+---+     +---+---+     +---+---+     +---+---+
; v ---> | | | ------> | | | ------> | | | ------> | | | / |
;        +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;          ↓             ↓             ↓             ↓
;         'a            'b            'c            'd       


;defining 'w' and representing 'v' and 'w' upon applying 'mystery'
;---
(define w (mystery v)) w
;---
;        +---+---+     +---+---+     +---+---+     +---+---+
; w ---> | | | ------> | | | ------> | | | ------> | | | / |
;        +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;          ↓             ↓             ↓             ↓
;         'd            'c            'b            'a  
;---
v
;---
;        +---+---+
; v ---> | | | / |
;        +-|-+---+
;          ↓
;         'a

;brief explanation of 'mystery'
;---
;simply put, 'mystery' returns the reverse of its input, while permanently modifying the
;latter. in the first iteration of 'loop', we set the 'cdr' of 'v' to be '(). hence, 'v'
;is modified to '(a). in subsequent iterations of 'loop', 'v' is no longer interacted
;with: from this point onwards, solely the previous iteration's 'temp' ('x' in the new
;frame) is modified, while 'temp' is "updated" by 'cdring' down the list. for a pictoreal
;explanation refer to Rather Iffy's response at "community.schemewiki.org/?sicp-ex-3.14".

