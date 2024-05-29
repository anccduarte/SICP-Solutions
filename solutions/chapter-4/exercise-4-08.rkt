
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.8.
;---
;"Named let" is a variant of 'let' that has the form
;---
;(let <var> <bindings> <body>)
;---
;The <bindings> and <body> are just as in ordinary 'let', except that <var> is bound
;within <body> to a procedure whose body is <body> and whose parameters are the variables
;in the <bindings>. Thus, one can repeatedly execute the <body> by invoking the procedure
;named <var>. For example, the iterative Fibonacci procedure (Section 1.2.2) can be
;rewritten using named 'let' as follows [see below *]. Modify 'let->combination' of
;Exercise 4.6 to also support named 'let'.
;------------------------------------------------------------------------------------------

;(*) 'fib' exploiting named 'let'
;---
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

;rewriting the syntax for 'let'
;---
(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (symbol? (cadr exp))) ;assumes 'let' expression
;---
(define (let-var exp) (cadr exp))
;---
(define (let-bindings exp)
  (if (named-let? exp) (caddr exp) (cadr exp)))
;---
(define (let-body exp)
  (if (named-let? exp) (cadddr exp) (caddr exp)))

;selectors for 'let' variables and values
;---
(define (let-variables exp)
  (let-extract (let-bindings exp) car))
;---
(define (let-values exp)
  (let-extract (let-bindings exp) cadr))
;---
(define (let-extract vars-vals op)
  (if (null? vars-vals)
      '()
      (cons (op (car vars-vals))
            (let-extract (cdr vars-vals) op))))

;auxiliary syntax
;[constructors for 'begin' and 'lambda' expressions]
;---
(define (make-begin . seq)
  (cons 'begin seq))
;---
(define (make-lambda vars body)
  (list 'lambda vars body))

;'vanilla-let->combination'
;[turning vanilla 'let' onto application of 'lambda' expression]
;---
(define (vanilla-let->combination exp)
  (cons (make-lambda (let-variables exp)
                     (let-body exp))
        (let-values exp)))

;'named-let->combination'
;[turning named 'let' onto 'begin' sequence -> define and call]
;---
(define (named-let->combination exp)
  (make-begin (list 'define
                    (let-var exp)
                    (make-lambda (let-variables exp)
                                 (let-body exp)))
              (cons (let-var exp) (let-values exp))))

;generic 'let->combination'
;[directing 'let' expression depending on the presence/absence of <var>]
;---
(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
      (vanilla-let->combination exp)))

;test
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;---
(let->combination '(let ((a 1)
                         (b 2)
                         (c 3))
                     (* a (+ b c))))
;---
(let->combination '(let fib-iter ((a 1)
                                  (b 0)
                                  (count 10))
                     (if (= count 0)
                         b
                         (fib-iter (+ a b) a (- count 1)))))

