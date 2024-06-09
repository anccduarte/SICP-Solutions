
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.20.
;---
;Because internal definitions look sequential but are actually simultaneous, some people
;prefer to avoid them entirely, and use the special form 'letrec' instead. 'letrec' looks
;like 'let', so it is not surprising that the variables it binds are bound simultaneously
;and have the same scope as each other. The sample procedure 'f' above [see below *] can
;be written without internal definitions, but with exactly the same meaning, as [see code
;below **]. 'letrec' expressions, which have the form
;---
;(letrec ((<var-1> <exp-1>) ... (<var-n> <exp-n>))
;  <body>)
;---
;are a variation on 'let' in which the expressions <exp-k> that provide the initial
;values for the variables <var-k> are evaluated in an environment that includes all the
;'letrec' bindings. This permits recursion in the bindings, such as the mutual recursion
;of 'even?' and 'odd?' in the example above [**], or the evaluation of 10 factorial with
;[see below ***].
;---
;(a) Implement 'letrec' as a derived expression, by transforming a 'letrec' expression
;    into a 'let' expression as shown in the text above or in Exercise 4.18. That is, the
;    'letrec' variables should be created with a 'let' and then be assigned their values
;    with 'set!'.
;---
;(b) Louis Reasoner is confused by all this fuss about internal definitions. The way he
;    sees it, if you don't like to use 'define' inside a procedure, you can just use
;    'let'. Illustrate what is loose about his reasoning by drawing an environment
;    diagram that shows the environment in which the <rest of body of f> is evaluated
;    during evaluation of the expression (f 5), with 'f' defined as in this exercise.
;    Draw an environment diagram for the same evaluation, but with 'let' in place of
;    'letrec' in the definition of 'f'.
;------------------------------------------------------------------------------------------

;(*) 'f' as defined in text
;---
(define (f-text x)
  (define (even? n) (if (= n 0) true (odd? (- n 1))))
  (define (odd? n) (if (= n 0) false (even? (- n 1))))
  'rest-of-body-of-f)

;(**) 'f' exploiting 'letrec'
;---
(define (f-letrec x)
  (letrec ((even? (lambda (n)
                    (if (= n 0) true (odd? (- n 1)))))
           (odd? (lambda (n)
                   (if (= n 0) false (even? (- n 1))))))
    'rest-of-body-of-f))

;(***) Evaluating 10 factorial by exploiting 'letrec'
;---
(letrec ((fact (lambda (n)
                 (if (< n 2) n (* n (fact (- n 1)))))))
  (fact 10))

;(a) Implemeting 'letrec' as a derived expression
;---
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
;---
(define (letrec-vars bindings) (map car bindings))
(define (letrec-vals bindings) (map cadr bindings))
;---
(define (make-let-from-letrec bindings
                              settings
                              body)
  (cons 'let
        (append (append (list bindings)
                        settings)
                body)))
;---
(define (transform-letrec exp)
  (let ((vars (letrec-vars (letrec-bindings exp)))
        (vals (letrec-vals (letrec-bindings exp))))
    (make-let-from-letrec
      (map (lambda (var) (list var (quote '*unassigned*)))
           vars)
      (map (lambda (var val) (list 'set! var val))
           vars vals)
      (letrec-body exp))))

;(a) Testing 'transform-letrec'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;---
(transform-letrec '(letrec ((even? (lambda (n)
                                     (if (= n 0) true (odd? (- n 1)))))
                            (odd? (lambda (n)
                                    (if (= n 0) false (even? (- n 1))))))
                     'rest-of-body-of-f))
;---
(let ((even? '*unassigned*)
      (odd? '*unassigned*))
  (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
  (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
  'rest-of-body-of-f)

;(b) Objecting Louis' reasoning
;---
;Note that Racket includes the primitives 'even?' and 'odd?', hence, for the interpreter
;to catch this somewhat sneaky error we should modify the names of 'even?' and 'odd?' in
;the above definitions to new names (we choose 'is-even?' and 'is-odd?', respectively).
;First, we write the definition of 'f' in terms of 'let' and then transform this same
;definition to its 'lambda' analog. Then, the 'lambda' equivalent of 'letrec' is layed
;out [see 'f-letrec' above].
;---
(define (f-let x)
  (let ((is-even? (lambda (n)
                    (if (= n 0) #t (is-odd? (- n 1)))))
        (is-odd? (lambda (n)
                   (if (= n 0) #t (is-even? (- n 1))))))
    'rest-of-body-of-f))
;---
(define (f-let-lambda x)
  ((lambda (is-even? is-odd?)
     'rest-of-body-of-f)
   (lambda (n)
     (if (= n 0) #t (is-odd? (- n 1))))
   (lambda (n)
     (if (= n 0) #t (is-even? (- n 1))))))
;---
(define (f-letrec-lambda x)
  ((lambda (is-even? is-odd?)
     (set! is-even? (lambda (n)
                      (if (= n 0) #t (is-odd? (- n 1)))))
     (set! is-odd? (lambda (n)
                     (if (= n 0) #t (is-even? (- n 1)))))
     'rest-of-body-of-f)
   '*unassigned*
   '*unassigned*))
;---
;Note the subtle distinction in the environment structure erected via calling each
;variant of 'f': in 'f-let-lambda', the procedures attributed to 'is-even?' and 'is-odd?'
;are defined in E1 although their names are bound in E2; on the other hand, the variant
;'f-letrec-lambda' both defines these procedures and binds their names in E2. By doing so,
;it is guaranteed that 'is-even?' has access to 'is-odd?' upon its definition and vice
;versa. The same does not apply for 'f-let-rec': since the names for the procedures are
;"concealed" in E2 while their definitions lie at E1, it is impossible for the latter to
;actually be aware of the existence of such names. "So, references to 'is-even?' inside
;'is-odd?' and vice versa will not resolve." [thanks to Mitchell Kember; his answer may
;be found at "https://mk12.github.io/sicp/exercise/4/1.html"]
;---
; global env [f] <--- E1 [x: 5] <--- E2 [is-even?, is-odd?]
;     ↑       |       ↑                     |         |       [USING
;     |       ↓       |                     ↓         ↓        'LET']
;     +---- [*|*]     +------------------ [*|*] --- [*|*]
;---
; global env [f] <--- E1 [x: 5] <--- E2 [is-even?, is-odd?]
;     ↑       |                      ↑      |         |       [USING
;     |       ↓                      |      ↓         ↓        'LETREC']
;     +---- [*|*]                    +--- [*|*] --- [*|*]

