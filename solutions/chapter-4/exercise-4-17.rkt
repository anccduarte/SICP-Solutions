
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.17.
;---
;Draw diagrams of the environment in effect when evaluating the expression <e3> in the
;procedure in the text, comparing how this will be structured when definitions are
;interpreted sequentially with how it will be structured if definitions are scanned out
;as described. Why is there an extra frame in the transformed program? Explain why this
;difference in environment structure can never make a difference in the behavior of a
;correct program. Design a way to make the interpreter implement the "simultaneous" scope
;rule for internal definitions without constructing the extra frame.
;------------------------------------------------------------------------------------------

;Sequential definitions
;---
(lambda (vars)
  (define u 'e1)
  (define v 'e2)
  'e3)
;---
;                  +--------+
; env defined ---> | vars   |
;  by lambda       | u: 'e1 | ['e3 is evaluated in
;                  | v: 'e2 |  this environment]
;                  +--------+

;Scanned out definitions
;---
;Note below that, in comparison to the previous method, an extra frame is constructed.
;This is the case since the current method for simultaneously evaluating definitions
;makes use of the special form 'let', which is no more than a syntax transformation of
;'lambda'. In turn, the evaluation of a 'lambda' expression yields an environment, thus
;the extra frame.
;---
;Nevertheless, despite the difference in environment structure, a correct program is to
;behave correctly by adhering to either one of the methods for defining procedures
;internally. Note that the only variables bound to the environment constructed by the
;'lambda' expression below are the ones defined upon the analogous sequential definition
;demonstrated above. Since, after executing the 'lambda' expression, these variables are
;set to exact same values as they would be set by sequential definition, any expression
;evaluated in either environment structure has access to equivalent (variable, value)
;pairs, and thus a correct program will behave identically by complying to either of the
;environment schemes.
;---
(lambda (vars)
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u 'e1)
    (set! v 'e2)
    'e3))
;---
(lambda (vars)
  ((lambda (u v)
     (set! u 'e1)
     (set! v 'e2)
     'e3)
   '*unassigned*
   '*unassigned*))
;---
;                  +--------+
; env defined ---> | vars   |
;  by lambda       +--------+
;                      ↑
;                  +------------------+
;                  | u: '*unassigned* | <--- env1
;                  | v: '*unassigned* |      [before assignments]
;                  +------------------+
;---
;                  +--------+
; env defined ---> | vars   |
;  by lambda       +--------+
;                      ↑
;                  +--------+
;                  | u: 'e1 | <--- env1
;                  | v: 'e2 |      [after assignments]
;                  +--------+      ['e3 is evaluated in
;                                   this environment]

;Alternative transformation
;---
;There is no need to construct an extra frame for performing the transformation. Defining
;the inner variables to the symbol '*unassigned* and, subsequent to all definitions,
;setting them to their actual value, serves the exact same purpose.
;---
(lambda (vars)
  (define u '*unassigned*)
  (define v '*unassigned*)
  (set! u 'e1)
  (set! v 'e2)
  'e3)

