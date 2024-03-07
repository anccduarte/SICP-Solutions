
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.12.
;---
;The following procedure for appending lists was introduced in Section 2.2.1:
;---
;(define (append x y)
;  (if (null? x)
;      y
;      (cons (car x) (append (cdr x) y))))
;---
;'append' forms a new list by successively 'consing' the elements of 'x' onto 'y'. The
;procedure 'append!' is similar to 'append', but it is a mutator rather than a
;constructor. It appends the lists by splicing them together, modifying the final pair of
;'x' so that its 'cdr' is now 'y'. (It is an error to call 'append!' with an empty 'x'.)
;---
;(define (append! x y)
;  (set-cdr! (last-pair x) y)
;  x)
;---
;Here 'last-pair' is a procedure that returns the last pair in its argument:
;---
;(define (last-pair x)
;  (if (null? (cdr x)) x (last-pair (cdr x))))
;---
;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))
;z -> (a b c d)
;(cdr x) -> <response>
;(define w (append! x y))
;w -> (a b c d)
;(cdr x) -> <response>
;---
;What are the missing <response>s? Draw box-and-pointer diagrams to explain your answer.
;------------------------------------------------------------------------------------------

;Footnote 17 (SICP's chapter 3)
;---
;"[...] mutation operations on lists can create 'garbage' that is not part of any
;accessible structure. We will see in Section 5.3.2. that Lisp memory-management systems
;include a 'garbage collector', which identifies and recycles the memory space used by
;unneeded pairs."

;'APPEND'
;---
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;'APPEND!'
;---
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
;---
(define (append! x y)
  (set-cdr! (last-pair x) y))

;TEST
;---
(define x (list 'a 'b))
(define y (list 'c 'd))
;---
(define z (append x y)) z ;(a b c d)
(cdr x) ;<response> -> (b)
;---
(define w (append! x y)) w ;(a b c d)
(cdr x) ;<response> -> (b c d)

;DIAGRAMS
;('x' after 'append' and 'append!') 
;---
;        +---+---+     +---+---+
; x ---> | | | ------> | | | / |
;        +-|-+---+     +-|-+---+
;          ↓             ↓
;         'a            'b
;---
;        +---+---+     +---+---+     +---+---+     +---+---+
; x ---> | | | ------> | | | ------> | | | ------> | | | / |
;        +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;          ↓             ↓             ↓             ↓
;         'a            'b            'c            'd

