
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.34.
;---
;Suppose we define the procedure (define (f g) (g 2)). Then we have (f square) -> 4 and
;(f (lambda (z) (* z (+ z 1)))) -> 6. What happens if we (perversely) ask the interpreter
;to evaluate the combination (f f)? Explain.
;------------------------------------------------------------------------------------------


;---
;ON 'LET' EXPRESSIONS
;---

;"The first part of the 'let' expression is a list of name-expression pairs. When the
;'let' is evaluated, each name is associated with the value of the corresponding
;expression. The body of the 'let' is evaluated with these names bound as local
;variables."

;"No new mechanism is required in the interpreter in order to provide local variables. A
;'let' expression is simply syntactic sugar for the underlying 'lambda' application. We
;can see from this equivalence that the scope of a variable specified by a 'let'
;expression is the body of the 'let'."

;"The variables' values are computed outside the 'let'. This matters when the expressions
;that provide the values for the local variables depend upon variables having the same
;names as the local variables themselves. For example, if the value of 'x' is 2, the
;expression
;(let ((x 3)
;      (y (+ x 2)))
;  (* x y))
;will have the value 12 because, inside the body of the 'let', 'x' will be 3 and y will
;be 4 (which is the outer 'x' plus 2)."


;---
;ANSWER TO EXERCISE
;---

;'f' is a procedure of one argument 'g' that applies 'g' to 2
;---
(define (f g) (g 2))

;tests above
;---
(define square (lambda (x) (* x x)))
(f square) ;(square 2) -> 4
(f (lambda (z) (* z (+ z 1)))) ;(* 2 (+ 2 1)) -> 6

;perversely evaluating (f f)
;---
;(f f) -> the second argument of 'f' ('f') is applied to 2
;(f 2) -> the second argument of 'f' (2) is applied to 2
;(2 2) -> error: application: not a procedure;
;                expected a procedure that can be applied to arguments
;                given: 2

