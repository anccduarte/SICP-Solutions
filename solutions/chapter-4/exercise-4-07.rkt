
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.7.
;---
;'let*' is similar to 'let', except that the bindings of the 'let*' variables are
;performed sequentially from left to right, and each binding is made in an environment in
;which all of the preceding bindings are visible. For example
;---
;(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
;  (* x z))
;---
;returns 39. Explain how a 'let*' expression can be rewritten as a set of nested 'let'
;expressions, and write a procedure 'let*->nested-lets' that performs this transformation.
;If we have already implemented 'let' (Exercise 4.6) and we want to extend the evaluator
;to handle 'let*', is it sufficient to add a clause to 'eval' whose action is
;---
;(eval (let*->nested-lets exp) env)
;---
;or must we explicitly expand 'let*' in terms of non-derived expressions?
;------------------------------------------------------------------------------------------

;writing 'let*' expression as a set of nested 'let' expressions
;[each 'let' expressions creates a 'lambda' expression, generating a new environment
;binding the 'lambda' parameters to the values/arguments it is called with; a nested set
;of 'let' expressions then constructs a series of 'lambda' expressions whose created
;environments are bound to the scope of their "predecessors"]
;---
(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
  (* x z))
;---
(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))

;syntax for 'let*'
;---
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-vars-values exp) (cadr exp))
(define (let*-body exp) (caddr exp))

;'let*->nested-lets'
;[there should be no problem in expanding 'let*' in terms of derived expressions, i.e.,
;'let'; since we have at our disposal procedures determining the syntax and evaluation of
;'let' (itself implemented as a syntactic transformation of 'lambda'), we let those
;procedures work in our favour to arrive at non-derived 'lambda' expressions, which are
;readily evaluated by 'eval'; such a strategy simplifies our language, as less evaluation
;procedures have to be constructed]
;---
(define (let*->nested-lets exp)
  (define (let*-iter vars-vals)
    (if (null? vars-vals)
        (let*-body exp)
        (make-let (list (car vars-vals))
                  (let*-iter (cdr vars-vals)))))
  (let*-iter (let*-vars-values exp)))

;testing 'let*->nested-lets'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;---
(define (make-let vars-vals body)
  (list 'let vars-vals body))
;---
(let*->nested-lets '(let* ((x 3)
                           (y (+ x 2))
                           (z (+ x y 5)))
                      (* x z)))

