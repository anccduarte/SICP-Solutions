
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.9.
;---
;In Section 1.2.1 we used the substitution model to analyze two procedures for computing
;factorials, a recursive version
;---
;(define (factorial n)
;  (if (= n 1) 1 (* n (factorial (- n 1)))))
;---
;and an iterative version
;---
;(define (factorial n) (fact-iter 1 1 n))
;(define (fact-iter product counter max-count)
;  (if (> counter max-count)
;      product
;      (fact-iter (* counter product)
;                 (+ counter 1)
;                 max-count)))
;---
;Show the environment structures created by evaluating (factorial 6) using each version
;of the factorial procedure.
;------------------------------------------------------------------------------------------

;USEFUL INSIGHTS from the book (3.2. The Environment Model of Evaluation, and 3.2.1. The
;Rules for Evaluation)
;---
;"(...) in the presence of assignment, a variable can no longer be considered to be
;merely a name for a value. Rather, a variable must somehow designate a "place" in which
;values can be stored. In our new model of evaluation, these places will be maintained in
;structures called environments."
;---
;"Indeed, one could say that expressions in a programming language do not, in themselves,
;have any meaning. Rather, an expression acquires a meaning only with respect to some
;environment in which it is evaluated."
;---
;"The environment model of procedure application can be summarized by two rules:
;- A procedure object is applied to a set of arguments by constructing a frame, binding
;the formal parameters of the procedure to the arguments of the call, and then evaluating
;the body of the procedure in the context of the new environment constructed. The new
;frame has as its enclosing environment the environment part of the procedure object
;being applied.
;- A procedure is created by evaluating a λ-expression relative to a given environment.
;The resulting procedure object is a pair consisting of the text of the λ-expression and
;a pointer to the environment in which the procedure was created."
;---
;"(...) defining a symbol using 'define' creates a binding in the current environment
;frame and assigns to the symbol the indicated value."
;---
;"Finally, we specify the behavior of 'set!', the operation that forced us to introduce
;the environment model in the first place. Evaluating the expression
;'(set! <variable> <value>)' in some environment locates the binding of the variable in
;the environment and changes that binding to indicate the new value. That is, one finds
;the first frame in the environment that contains a binding for the variable and modifies
;that frame. If the variable is unbound in the environment, then 'set!' signals an
;error."

;SOLUTION (for reasons of clarity, we show the environment structures created by
;evaluating '(factorial 3)' - in the recursive version - and '(factorial 2)' - in the
;iterative version - instead of '(factorial 6)')
;---
; global ---> +----------------------------------------------------------------+
;  env        | factorial                                                      |
;             |    |                                                           |
;             +----|-----------------------------------------------------------+
;                  |      ↑               ↑               ↑               ↑
;                  ↓      |               |               |               |
;              +---+---+  |    +-------+  |    +-------+  |    +-------+  |
;              | | | -----+    | n: 3  | -+    | n: 2  | -+    | n: 1  | -+
;              +-|-+---+       +-------+       +-------+       +-------+
;                |                 ↑               ↑               ↑
;                ↓                 |               |               |
;           parameters: n      E1 -+           E2 -+           E3 -+
;           body: (...)
;---
; global ---> +-----------------------------------------------------------------------+
;  env        | factorial     fact-iter                                               |
;             |    |              |                                                   |
;             +----|--------------|---------------------------------------------------+
;                  |      ↑       |      ↑             ↑                      ↑    ↑
;                  ↓      |       ↓      |             |                      |    |
;              +---+---+  |   +-------+  |   +------+  |   +---------------+  |    |
;              | | | -----+   |   | -----+   | n: 2 | -+   | product: 1    |  |    |
;              +-|-+---+      +-------+      +------+      | counter: 1    | -+    |
;                |                |               ↑        | max-count: 2  |       |
;                ↓                ↓               +- E1    +---------------+       |
;            parameters: n   parameters: product                        ↑          |
;            body: (...)                 counter                        +- E2      |
;                                        max-count                                 |
;                            body: (...)                        +---------------+  |
;                                                               | product: 2    |  |
;                                                               | counter: 2    | -+
;                                                               | max-count: 2  |
;                                                               +---------------+
;                                                                            ↑
;                                                                            +- E3

