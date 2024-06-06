
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.16.
;---
;In this exercise we implement the method just described for interpreting internal
;definitions. We assume that the evaluator supports 'let' (see Exercise 4.6).
;---
;(a) Change 'lookup-variable-value' (Section 4.1.3) to signal an error if the value it
;    finds is the symbol *unassigned*.
;---
;(b) Write a procedure 'scan-out-defines' that takes a procedure body and returns an
;    equivalent one that has no internal definitions, by making the transformation
;    described above.
;---
;(c) Install 'scan-out-defines' in the interpreter, either in 'make-procedure' or in
;    'procedure-body' (see Section 4.1.3). Which place is better? Why?
;------------------------------------------------------------------------------------------

;lambdas and definitions
;---
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
;---
(define (definition? exp) (tagged-list? exp 'define))
;---
(define (define-variables exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
;---
(define (define-values exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;frames and environments
;---
(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))
;---
(define (make-frame vars vals) (cons vars vals))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

;(a) 'lookup-variable-value'
;---
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan variables values)
      (cond ((null? variables)
             (env-loop (enclosing-environment env)))
            ((eq? var (car variables))
             (let ((val (car values)))
               (if (eq? val '*unassigned*)
                   (error "Variable used before assignment:" var)
                   val)))
            (else
             (scan (cdr variables) (cdr values)))))
    (if (eq? env the-empty-environment)
        (error "Variable not defined:" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

;(b) 'scan-out-defines'
;[three assumptions are made when performing the syntactic transformation: 1. internal
;definitions come first in the body of the procedure; 2. no internal definition actually
;makes use of any of the internally defined variables; 3. a body of a procedure must
;imperatively contain at least one expression other than a definition]
;---
(define (scan-out-defines proc-body)
  ;---
  (define (extract-defines proc-body)
    (if (definition? (car proc-body))
        (cons (car proc-body)
              (extract-defines (cdr proc-body)))
        '()))
  ;---
  (define (extract-expressions proc-body)
    (if (definition? (car proc-body))
        (extract-expressions (cdr proc-body))
        proc-body))
  ;---
  (define (convert-to-let inner-defines expressions)
    (let ((variables (map define-variables inner-defines))
          (values (map define-values inner-defines)))
      (list 'let
            (map (lambda (var) (list var (quote '*unassigned*)))
                 variables)
            (cons 'begin
                  (append (map (lambda (var val) (list 'set! var val))
                               variables values)
                          expressions)))))
  ;---
  (let ((inner-defines (extract-defines proc-body))
        (expressions (extract-expressions proc-body)))
    (if (null? inner-defines)
        expressions
        (convert-to-let inner-defines expressions))))


;(b) testing 'scan-out-defines'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;---
(let ((proc-body '((define (test1 arg) (< arg 0))
                   (define (test2 arg) arg)
                   (define (print r) (display r) (newline))
                   (print (test1 4))
                   (print (test2 56)))))
  (scan-out-defines proc-body))
;---
(let ((test1 '*unassigned*)
      (test2 '*unassigned*)
      (print '*unassigned*))
  (begin (set! test1 (lambda (arg) (< arg 0)))
         (set! test2 (lambda (arg) arg))
         (set! print (lambda (r) (display r) (newline)))
         (print (test1 4))
         (print (test2 56))))

;(c) where to install 'scan-out-defines'?
;---
;in the present context, it would be better to install 'scan-out-defines' at construction
;time (i.e., 'make-procedure') rather than at selection time (i.e., 'procedure-body');
;this is the case since we assume that once a procedure is defined it will be exploited
;at least once, but most probably multiple times; by installing the syntax transformation
;at construction time, we guarantee that it is performed only once; on the contrary, by
;establishing it at 'procedure-body', our programs will probably become less efficient,
;since the transformation may be carried out several times
;---
(define (make-procedure parameters body env)
  (list 'procedure
        parameters
        (scan-out-defines body)
        env))

