
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.16.
;---
;Ben Bitdiddle decides to write a procedure to count the number of pairs in any list
;structure. "It's easy", he reasons. "The number of pairs in any structure is the number
;in the 'car' plus the number in the 'cdr' plus one more to count the current pair." So
;Ben writes the following procedure:
;---
;(define (count-pairs x)
;  (if (not (pair? x))
;      0
;      (+ (count-pairs (car x))
;         (count-pairs (cdr x))
;         1)))
;---
;Show that this procedure is not correct. In particular, draw box-and-pointer diagrams
;representing list structures made up of exactly three pairs for which Ben's procedure
;would return 3; return 4; return 7; never return at all.
;------------------------------------------------------------------------------------------

;Ben's 'count-pairs'
;(the procedure is actually correct if we do not account for sharing and mutability)
;---
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;returns 3 pairs
;---
(define r3 '(a b c))
(count-pairs r3)
;---
;         +---+---+     +---+---+     +---+---+
; r3 ---> | | | ------> | | | ------> | | | / |
;         +-|-+---+     +-|-+---+     +-|-+---+
;           ↓             ↓             ↓
;          'a            'b            'c

;returns 4 pairs
;(based on Anon's answer -> community.schemewiki.org/?sicp-ex-3.16)
;---
(define x '(a))
(define y (cons x x))
(define r4 (list y))
(count-pairs r4)
;---
;         +---+---+
; r4 ---> | | | / |
;         +-|-+---+
;           ↓
;         +---+---+
;  y ---> | | | | |
;         +-|-+-|-+
;           ↓   ↓
;         +---+---+
;  x ---> | | | / |
;         +-|-+---+
;           ↓
;          'a

;returns 7 pairs
;(based on Anon's answer -> community.schemewiki.org/?sicp-ex-3.16)
;---
(define r7 (cons y y))
(count-pairs r7)
;---
;         +---+---+
; r7 ---> | | | | |
;         +-|-+-|-+
;           ↓   ↓
;         +---+---+
;  y ---> | | | | |
;         +-|-+-|-+
;           ↓   ↓
;         +---+---+
;  x ---> | | | / |
;         +-|-+---+
;           ↓
;          'a

;never returns
;---
(define (last-pair x) (if (null? (cdr x)) x (last-pair (cdr x))))
(define (make-cycle x) (set-cdr! (last-pair x) x) x)
(define t '(a b c))
(define nr (make-cycle t))
;---
;  t ---> +---+---+     +---+---+     +---+---+
; nr ---> | | | ------> | | | ------> | | | -------+
;  +----> +-|-+---+     +-|-+---+     +-|-+---+    |
;  |        ↓             ↓             ↓          |
;  |       'a            'b            'c          |
;  +-----------------------------------------------+

