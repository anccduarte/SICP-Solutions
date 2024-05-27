
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.6.
;---
;'let' expressions are derived expressions, because
;---
;(let ((<var1> <exp1>) ... (<varn> <expn>))
;  <body>)
;---
;is equivalent to
;---
;((lambda (<var1> ... <varn>)
;   <body>)
; <exp1>
; ...
; <expn>)
;---
;Implement a syntactic transformation 'let->combination' that reduces evaluating 'let'
;expressions to evaluating combinations of the type shown above, and add the appropriate
;clause to 'eval' to handle 'let' expressions.
;------------------------------------------------------------------------------------------

;synatx for 'let'
;---
(define (let? exp) (tagged-list? exp 'let))
(define (let-vars-values exp) (cadr exp))
(define (let-body exp) (caddr exp))
;---
(define (let-extract vars-vals op)
  (if (null? vars-vals)
      '()
      (cons (op (car vars-vals))
            (let-extract (cdr vars-vals) op))))
;---
(define (let-vars vv) (let-extract vv car))
(define (let-vals vv) (let-extract vv cadr))

;'let->combination' [syntactic transformation of 'let' onto 'lambda']
;---
(define (let->combination exp)
  (list (make-lambda
         (let-vars (let-vars-values exp))
         (let-body exp))
        (let-vals (let-vars-values exp))))

;testing 'let->combination'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;---
(define (make-lambda params body)
  (list 'lambda params body))
;---
(let->combination '(let ((a 1)
                         (b 2)
                         (c 3))
                     (* a (+ b c))))

