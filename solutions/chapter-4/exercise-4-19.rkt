
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.19.
;---
;Ben Bitdiddle, Alyssa P. Hacker, and Eva Lu Ator are arguing about the desired result of
;evaluating the expression
;---
;(let ((a 1))
;  (define (f x)
;    (define b (+ a x))
;    (define a 5)
;    (+ a b))
;  (f 10))
;---
;Ben asserts that the result should be obtained using the sequential rule for define: 'b'
;is defined to be 11, then 'a' is defined to be 5, so the result is 16. Alyssa objects
;that mutual recursion requires the simultaneous scope rule for internal procedure
;definitions, and that it is unreasonable to treat procedure names differently from other
;names. Thus, she argues for the mechanism implemented in Exercise 4.16. This would lead
;to 'a' being unassigned at the time that the value for 'b' is to be computed. Hence, in
;Alyssa's view the procedure should produce an error. Eva has a third opinion. She says
;that if the definitions of 'a' and 'b' are truly meant to be simultaneous, then the
;value 5 for 'a' should be used in evaluating 'b'. Hence, in Eva's view 'a' should be 5,
;'b' should be 15, and the result should be 20. Which (if any) of these viewpoints do you
;support? Can you devise a way to implement internal definitions so that they behave as
;Eva prefers? [see footnote *]
;------------------------------------------------------------------------------------------

;(*) Footnote 26
;---
;"The MIT implementors of Scheme support Alyssa on the following grounds: Eva is in
;principle correct — the definitions should be regarded as simultaneous. But it seems
;difficult to implement a general, efficient mechanism that does what Eva requires. In
;the absence of such a mechanism, it is better to generate an error in the difficult
;cases of simultaneous definitions (Alyssa's notion) than to produce an incorrect answer
;(as Ben would have it)."

;Supported viewpoint
;---
;Contrary to the authors' opinion [run the following expression and observe the error it
;produces], I'm inclined to think that, for intelligibility reasons, the expression may
;be evaluated as Ben suggests, that is, when defining 'b', 'lookup-variable-value' should
;be looking for the "closest" definition of 'a' it can find in the environment (in this
;case, the 'a' bound by the 'let' expression). However, such a view has the disadvantage
;of completely discarding the concept of simultaneous internal definitions, which may be
;troublesome in some cases [see Exercise 4.18 for such an example].

;Implementing Eva's intended behavior
;---
;Note that the present implementation is fallible. The idea is the following: when
;scanning out the inner definitions, one should ask "Does the variable being defined
;explicitly depends on an unassigned variable for its definition?". If so, it is kept on
;hold and we move on to the next definition. Otherwise, the variable is defined and the
;next definition is evaluated. Such an approach may be problematic. If we complete a loop
;over the remaining defines without actually defining any new variable, we reach an
;impasse: no more varibales can be defined and the program should return an error.
;---
(define (reconfigure-defines proc-body)
  ;---
  (define (extract-defines proc-body)
    (if (not (definition? (car proc-body)))
        '()
        (cons (car proc-body)
              (extract-defines (cdr proc-body)))))
  ;---
  (define (extract-expressions proc-body)
    (if (not (definition? (car proc-body)))
        proc-body
        (extract-expressions (cdr proc-body))))
  ;---
  (define (depends-on-failed? exp fail-vars)
    (if (null? exp)
        #f
        (let ((first (car exp))
              (rest (cdr exp)))
          (if (or (lambda? first) (delay? first))
              (depends-on-failed? rest fail-vars)
              (or (if (not (pair? first))
                      (memq first fail-vars)
                      (depends-on-failed? first fail-vars))
                  (depends-on-failed? rest fail-vars))))))
  ;---
  (define (add exp exps)
    (if (null? exps)
        (cons exp '())
        (cons (car exps)
              (add exp (cdr exps)))))
  ;---
  (define (remove exp exps)
    (let ((var-exp (define-variable exp))
          (var-first-exp (define-variable (car exps))))
      (if (eq? var-exp var-first-exp)
          (cdr exps)
          (cons (car exps)
                (remove exp (cdr exps))))))
  ;---
  (let* ((inner-defines (extract-defines proc-body))
         (previous-length -1))
    ;---
    (define (reconfigure remaining successes failures)
      (cond ((= (length successes) (length inner-defines))
             successes)
            ((null? remaining)
             (if (= (length failures) previous-length)
                 (error "Cannot perform simultaneous definitions!")
                 (begin (set! previous-length (length failures))
                        (reconfigure failures successes failures))))
            (else
             (let ((d-exp (car remaining)))
               (if (depends-on-failed? d-exp
                                       (map define-variable
                                            (remove d-exp failures)))
                   (reconfigure (cdr remaining) successes failures)
                   (reconfigure (cdr remaining)
                                (add d-exp successes)
                                (remove d-exp failures)))))))
    ;---
    (append (reconfigure inner-defines '() inner-defines)
            (extract-expressions proc-body))))

;Testing 'reconfigure-defines'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;---
(define (definition? exp) (tagged-list? exp 'define))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (delay? exp) (tagged-list? exp 'delay))
;---
(define (define-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
;---
(define (test proc-body)
  (display "original body: ")
  (display proc-body) (newline)
  (display "reconfigured body: ")
  (display (reconfigure-defines proc-body)) (newline)
  (display "---") (newline))
;---
(let ((proc-body-1 '((define b (+ a x))
                     (define a 5)
                     (+ a b)))
      ;---
      (proc-body-2 '((define dy (stream-map f y))
                     (define y (integral (delay dy) y0 dt))
                     y)))
  ;---
  (display "---") (newline)
  (test proc-body-1)
  (test proc-body-2))

