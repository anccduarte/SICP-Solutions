
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.11.
;---
;Instead of representing a frame as a pair of lists, we can represent a frame as a list
;of bindings, where each binding is a name-value pair. Rewrite the environment operations
;to use this alternative representation.
;------------------------------------------------------------------------------------------

;abstracting environments
;---
(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

;abstracting frames
;---
(define (make-frame vars vals)
  (if (not (= (length vars) (length vals)))
      (error "Dimensionality complication!")
      (cons '*frame*
            (let lp ((vars vars)
                     (vals vals))
              (if (null? vars)
                  '()
                  (cons (cons (car vars) (car vals))
                        (lp (cdr vars) (cdr vals))))))))
;---
(define (frame-contents frame) (cdr frame))
;---
(define (first-pair frame) (car frame))
(define (rest-pairs frame) (cdr frame))
;---
(define (add-binding-to-frame! var val frame)
  (let ((new-binding (cons var val)))
    (set-cdr! frame (cons new-binding (cdr frame)))))

;'extend-environment'
;[no modifications are needed in comparison to the original version]
;---
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;'lookup-variable-value'
;---
(define (lookup-variable-value var env)
  (define (scan frame)
    (cond ((null? frame)
           false)
          ((eq? var (car (first-pair frame)))
           (cdr (first-pair frame)))
          (else
           (scan (rest-pairs frame)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable:" var)
      (let ((val
             (scan (frame-contents (first-frame env)))))
        (or val
            (lookup-variable-value var
                                   (enclosing-environment env))))))

;'set-variable-value!'
;---
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (first-pair frame)))
             (set-cdr! (first-pair frame) val))
            (else
             (scan (rest-pairs frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable:" var)
        (scan (frame-contents (first-frame env)))))
  (env-loop env))

;'define-variable!'
;---
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let lp ((cframe (frame-contents frame)))
      (cond ((null? cframe)
             (add-binding-to-frame! var val frame))
            ((eq? var (car (first-pair cframe)))
             (set-cdr! (first-pair cframe) val))
            (else
             (lp (rest-pairs cframe)))))))

;testing alternative representation
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

