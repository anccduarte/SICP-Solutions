
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.15.
;---
;Draw box-and-pointer diagrams to explain the effect of 'set-to-wow!' on the structures
;'z1' and 'z2' above. [See definitions below.]
;------------------------------------------------------------------------------------------

;footnote 19 (chapter 3)
;---
;"(...) each call to 'cons' returns a new pair. (...) symbols are shared; in Scheme there
;is a unique symbol with any given name. Since Scheme provides no way to mutate a symbol,
;this sharing is undetectable. Note also that the sharing is what enables us to compare
;symbols using 'eq?', which simply checks equality of pointers."
;---
;(define a 'a)
;(define b (cons a 'b))
;b -> (a . b)
;(set! a 'c)
;b -> still evaluates to (a . b) even though 'a' was modified

;note on sharing
;---
;"In general, sharing is completely undetectable if we operate on lists using only
;'cons', 'car', and 'cdr'. However, if we allow mutators on list structure, sharing
;becomes significant."

;another note on sharing (see footnote 19)
;---
;"One way to detect sharing in list structures is to use the predicate 'eq?', which we
;introduced in Section 2.3.1 as a way to test whether two symbols are equal. More
;generally, (eq? x y) tests whether 'x' and 'y' are the same object (that is, whether 'x'
;and 'y' are equal as pointers)."

;a final note on sharing and mutability
;---
;"The mutation operations 'set-car!' and 'set-cdr!' should be used with care; unless we
;have a good understanding of how our data objects are shared, mutation can have
;unanticipated results."

;SOLUTION
;- 1. 'z1' before 'set-to-wow!'
;- 2. 'z1' after 'set-to-wow!'
;- 3. 'z2' before 'set-to-wow!'
;- 4. 'z2' after 'set-to-wow!'
;---
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
;---
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
;---
z1 (set-to-wow! z1)
z2 (set-to-wow! z2)
;---
;         +---+---+
; z1 ---> | | | | |
;         +-|-+-|-+
;           ↓   ↓
;         +---+---+     +---+---+
;  x ---> | | | ------> | | | / |
;         +-|-+---+     +-|-+---+
;           ↓             ↓
;          'a            'b
;---
;         +---+---+
; z1 ---> | | | | |
;         +-|-+-|-+
;           ↓   ↓
;         +---+---+     +---+---+
;  x ---> | | | ------> | | | / |
;         +-|-+---+     +-|-+---+
;           ↓             ↓
;         'wow           'b
;---
;         +---+---+     +---+---+     +---+---+
; z2 ---> | | | ------> | | | ------> | | | / |
;         +-|-+---+     +-|-+---+     +-|-+---+
;           |             ↓             ↓
;           |            'a            'b
;           |             ↑             ↑
;           |           +-|-+---+     +-|-+---+
;           +---------> | | | ------> | | | / |
;                       +---+---+     +---+---+
;---
;         +---+---+     +---+---+     +---+---+
; z2 ---> | | | ------> | | | ------> | | | / |
;         +-|-+---+     +-|-+---+     +-|-+---+
;           |             ↓             ↓
;           |           'wow           'b
;           |                           ↑
;           |           +---+---+     +-|-+---+
;           +---------> | | | ------> | | | / |
;                       +-|-+---+     +---+---+
;                         ↓
;                        'a

