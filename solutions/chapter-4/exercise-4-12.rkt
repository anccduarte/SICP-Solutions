
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.12.
;---
;The procedures 'set-variable-value!', 'define-variable!' and 'lookup-variable-value' can
;be expressed in terms of more abstract procedures for traversing the environment
;structure. Define abstractions that capture the common patterns and redefine the three
;procedures in terms of these abstractions.
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;REPRESENTING AND EXTENDING ENVIRONMENTS
;[common to both implementation alternatives below]
;------------------------------------------------------------------------------------------

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

;'extend-environment'
;---
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


;------------------------------------------------------------------------------------------
;MY IMPLEMENTATION
;[maybe a bit confusing]
;------------------------------------------------------------------------------------------

;'scan-frame'
;[a higher-order procedure taking two procedures as input: 'end-frame' specifies what to
;do if the end of a frame (i.e., 'frame' is null) is reached, and 'found-var' defining
;the action to take whenever 'var' is found in the frame]
;---
(define (scan-frame var frame end-frame found-var)
  (let ((vars (frame-variables frame))
        (vals (frame-values frame)))
    (let lp ((vars vars) (vals vals))
      (cond ((null? vars) (end-frame frame))
            ((eq? var (car vars)) (found-var vals))
            (else (lp (cdr vars) (cdr vals)))))))

;'iter-env'
;---
(define (iter-env var env end-frame found-var)
  (if (eq? env the-empty-environment)
      (error "Unbound variable:" var)
      (let ((res (scan-frame var
                             (first-frame env)
                             end-frame
                             found-var)))
        (or res
            (iter-env var
                      (enclosing-environment env)
                      end-frame
                      found-var)))))

;'my-lookup-variable-value'
;---
(define (my-lookup-variable-value var env)
  (iter-env var
            env
            (lambda (frame) #f)
            (lambda (vals) (car vals))))

;'my-set-variable-value!'
;---
(define (my-set-variable-value! var val env)
  (iter-env var
            env
            (lambda (frame) #f)
            (lambda (vals) (set-car! vals val))))

;'my-define-variable!'
;---
(define (my-define-variable! var val env)
  (scan-frame var
              (first-frame env)
              (lambda (frame)
                (add-binding-to-frame! var val frame))
              (lambda (vals) (set-car! vals val))))


;------------------------------------------------------------------------------------------
;ALTERNATIVE IMPLEMETATION
;[credit to Mitchell Kember for the brilliant solution; the original post may be visited
;at https://mk12.github.io/sicp/exercise/4/1.html]
;------------------------------------------------------------------------------------------

;'traverse'
;---
(define (traverse var env action otherwise)
  (if (eq? env the-empty-environment)
      (error 'traverse "Unbound variable" var)
      (let ((frame (first-frame env)))
        (let lp ((vars (frame-variables frame))
                 (vals (frame-values frame)))
          (cond ((null? vars)
                 (otherwise frame (enclosing-environment env)))
                ((eq? var (car vars))
                 (action vals))
                (else
                 (lp (cdr vars) (cdr vals))))))))

;'lookup-variable-value'
;---
(define (lookup-variable-value var env)
  (traverse var
            env
            (lambda (vals) (car vals))
            (lambda (frame env) (lookup-variable-value var env))))

;'set-variable-value!'
;---
(define (set-variable-value! var val env)
  (traverse var
            env
            (lambda (vals) (set-car! vals val))
            (lambda (frame env) (set-variable-value! var val env))))

;'define-variable!'
;---
(define (define-variable! var val env)
  (traverse var
            env
            (lambda (vals) (set-car! vals val))
            (lambda (frame env) (add-binding-to-frame! var val frame))))

;testing
;---
(define (print name-obj obj)
  (display name-obj) (display " -> ")
  (display obj) (newline))
;---
(let ((frame (make-frame '(a b c d) '(1 2 3 4))))
  (print "frame [before adding]" frame)
  (add-binding-to-frame! 'e 5 frame)
  (print "frame [after adding]" frame))
;---
(display "---") (newline)
;---
(let* ((env1 the-empty-environment)
       (env2 (extend-environment '(a b) '(1 2) env1))
       (env3 (extend-environment '(c d) '(3 4) env2)))
  (print "env1" env1)
  (print "env2" env2)
  (print "env3" env3)
  (print "a [before setting]" (lookup-variable-value 'a env3))
  (set-variable-value! 'a 5 env3)
  (print "a [after setting]" (lookup-variable-value 'a env3))
  (define-variable! 'e 6 env2)
  (print "env2" env2)
  (print "env3" env3))

