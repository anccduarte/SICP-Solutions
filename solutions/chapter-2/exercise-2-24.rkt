
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.24.
;---
;Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the result printed
;by the interpreter, the corresponding box-and-pointer structure, and the interpretation
;of this as a tree (as in Figure 2.6).
;------------------------------------------------------------------------------------------

;expression
;---
(list 1 (list 2 (list 3 4)))

;result printed by the interpreter
;---
;(1 (2 (3 4)))

;box-and-pointer structure
;(inspired by jz's solution -> http://community.schemewiki.org/?sicp-ex-2.24)
;---
;     (1 (2 (3 4)))  ((2 (3 4)))
;       +-+-+-+-+     +-+-+-+-+
; ----> | * | *-+---> | * | / |
;       +-+-+-+-+     +-+-+-+-+
;         |             |
;         v             v (2 (3 4))  ((3 4))
;       +-+-+         +-+-+-+-+     +-+-+-+-+
;       | 1 |         | * | *-+---> | * | / |
;       +-+-+         +-+-+-+-+     +-+-+-+-+
;                       |             |
;                       v             v (3 4)        (4)
;                     +-+-+         +-+-+-+-+     +-+-+-+-+
;                     | 2 |         | * | *-+---> | * | / |
;                     +-+-+         +-+-+-+-+     +-+-+-+-+
;                                     |             |
;                                     v             v
;                                   +-+-+         +-+-+
;                                   | 3 |         | 4 |
;                                   +-+-+         +-+-+

;intepretation as a tree
;---
; (1 (2 (3 4)))
;    /     \
;   1    (2 (3 4))
;         /   \
;        2   (3 4)
;             / \
;            3   4

