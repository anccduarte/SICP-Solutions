
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.14.
;---
;Eva Lu Ator and Louis Reasoner are each experimenting with the metacircular evaluator.
;Eva types in the definition of 'map', and runs some test programs that use it. They work
;fine. Louis, in contrast, has installed the system version of 'map' as a primitive for
;the metacircular evaluator. When he tries it, things go terribly wrong. Explain why
;Louis's 'map' fails even though Eva's works.
;------------------------------------------------------------------------------------------

;Eva's experiment
;---
;Eva does not install 'map' as a primitive for the metacircular evaluator. Eva's version
;of 'map' [the variant below assumes two arguments: a procedure and a list object] simply
;overwrites the underlying Lisp system's 'map' and, assuming the new representation for
;'map' is valid, the programs using should work fine.
;---
;(define (map proc lst)
;  (if (null? lst)
;      '()
;      (cons (proc (car lst))
;            (map proc (cdr lst)))))
;---
;For example (map car '((1 2 3) (4 5 6))) yields '(1 4) by exploiting either Eva's or the
;underlying Lisp system's variants of 'map'.

;Louis's experiment
;---
;Louis's approach fails because 'map' is a higher-order procedure (i.e., one of its
;arguments is itself a procedure). In such a circumstance, mixing the capabilities of
;the native Lisp and the new language being implemented in terms of the former becomes
;quite dangerous. This is due to the fact that a procedure does not solely correspond to
;the respective specification, but it also includes a type tag (either 'primitive or
;'compound-procedure). Procedures such as 'cons', 'car' and 'cdr' are immune to this
;difficulty, since their evaluation does not include planting a "furtive" type tag on the
;respective arguments. Thus, the native 'apply' primitive works correctly. Let's examine
;the evolution of a process originated via calling Louis' 'map' and see where it breaks.
;---
;(eval (map + '(1 2) '(3 4)) global-environment)
;---
;(apply (eval 'map global-environment)
;       (list-of-values '(+ (1 2) (3 4)) global-environment))
;---
;(apply '(primitive map)
;       '((primitive +) (1 2) (3 4)))
;---
;(apply-primitive-procedure '(primitive map)
;                           '((primitive +) (1 2) (3 4)))
;---
;(apply-in-underlying-scheme map
;                            '((primitive +) (1 2) (3 4)))
;---
;Note that 'apply-in-underlying-scheme' is just an alias for the underlying Lisp 'apply'.
;Hence, we are essentially trying to apply 'map' to a list object whose first element is
;'(primitive +) instead of the operation '+' itself. An error is obviously thrown:
;---
;"application: not a procedure;
; expected a procedure that can be applied to arguments
; given: (primitive #<procedure:+>)"

