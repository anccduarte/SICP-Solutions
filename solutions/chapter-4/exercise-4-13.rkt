
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.13.
;---
;Scheme allows us to create new bindings for variables by means of 'define', but provides
;no way to get rid of bindings. Implement for the evaluator a special form
;'make-unbound!' that removes the binding of a given symbol from the environment in which
;the 'make-unbound!' expression is evaluated. This problem is not completely specified.
;For example, should we remove only the binding in the first frame of the environment?
;Complete the specification and justify any choices you make.
;------------------------------------------------------------------------------------------

;'make-unbound!' specification
;---
;1. remove the binding of a given symbol from the specified environment;
;2. raise error if the symbol isn't found in the environment;
;3. solely remove the first occurrence of the symbol in the environment (i.e., the
;   process reaches an end whenever the symbol is found and removed from a given frame)
;---
;note that alternative implementations are just as valid as the suggested by the
;specification; for example, a more cautious, less error-prone approach would dictate
;that bindings should only be removed from the first frame of the designated environment;
;a more "indulgent" method would remove the bindings of a given symbol from all frames
;composing the environment

;representing environments
;---
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;representing frames
;---
(define (make-frame variables values)
  (cons variables values))
;---
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
;---
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
;---
(define (remove-binding-from-frame! pos frame)
  (define (remove count lst)
    (if (= count pos)
        (cdr lst)
        (cons (car lst)
              (remove (+ count 1) (cdr lst)))))
  (set-car! frame (remove 0 (frame-variables frame)))
  (set-cdr! frame (remove 0 (frame-values frame))))

;'extend-environment'
;---
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;'make-unbound!'
;---
(define (make-unbound! var env)
  ;---
  (define (scan-frame frame)
    (define (scan vars pos)
      (cond ((null? vars)
             (make-unbound! var (enclosing-environment env)))
            ((eq? var (car vars))
             (remove-binding-from-frame! pos frame))
            (else
             (scan (cdr vars) (+ pos 1)))))
    (scan (frame-variables frame) 0))
  ;---
  (if (eq? env the-empty-environment)
      (error "Unbound variable:" var)
      (scan-frame (first-frame env))))

;testing
;---
(define (print name-obj obj)
  (display name-obj) (display " -> ")
  (display obj) (newline))
;---
(let* ((env1 the-empty-environment)
       (env2 (extend-environment '(a b) '(1 2) env1))
       (env3 (extend-environment '(c d) '(3 4) env2)))
  (print "env1" env1)
  (print "env2" env2)
  (print "env3" env3)
  (make-unbound! 'a env3)
  (print "env3 [unbound 'a]" env3)
  (make-unbound! 'b env3)
  (print "env3 [unbound 'b]" env3)
  (make-unbound! 'c env3)
  (print "env3 [unbound 'c]" env3)
  (make-unbound! 'd env3)
  (print "env3 [unbound 'd]" env3))

