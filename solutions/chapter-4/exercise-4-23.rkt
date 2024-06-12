
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.23.
;---
;Alyssa P. Hacker doesn't understand why 'analyze-sequence' needs to be so complicated.
;All the other analysis procedures are straightforward transformations of the
;corresponding evaluation procedures (or 'eval' clauses) in Section 4.1.1. She expected
;'analyze-sequence' to look like this [see below *].
;---
;Eva Lu Ator explains to Alyssa that the version in the text does more of the work of
;evaluating a sequence at analysis time. Alyssa's sequence-execution procedure, rather
;than having the calls to the individual execution procedures built in, loops through the
;procedures in order to call them: In effect, although the individual expressions in the
;sequence have been analyzed, the sequence itself has not been.
;---
;Compare the two versions of 'analyze-sequence'. For example, consider the common case
;(typical of procedure bodies) where the sequence has just one expression. What work will
;the execution procedure produced by Alyssa's program do? What about the execution
;procedure produced by the program in the text above? How do the two versions compare for
;a sequence with two expressions?
;------------------------------------------------------------------------------------------

;Original version of 'analyze-sequence'
;---
;Suppose the input sequence to 'analyze-sequence' is composed by a single expression. In
;such a circumstance, 'analyze-sequence' would return an execution procedure consisting
;of the execution procedure resulting from analyzing the expression it took as input. Now,
;assume that the input sequence encompasses two expressions. In this case, the analysis
;procedure would call 'loop' twice, producing a 'lambda' expression (not nested at this
;point) having 'env' as a parameter and calling both execution procedures (those produced
;by analyzing the pair of expressions) with 'env' as argument. A sequence consisting of n
;expressions (with n > 2) behaves similarly by calling 'loop' n times and constructing a
;procedure (a 'lambda' expression) whose number of nesting levels in n-1. The key point
;to take from such a litany is that the original version of 'analyze-sequence', no matter
;the number of expressions it is "fed" with, always returns an already assembled 'lambda'
;expression that, upon receiving an environment, sequentially runs the enclosed execution
;procedures without looping. That is, the looping takes place at analysis time, rather
;than at evaluation time.
;---
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

;(*) Alyssa's version of 'analyze-sequence'
;---
;Alyssa's procedure is less efficient than the one originally exposed. Although its
;behavior its identical to the latter when considering sequences consisting of a single
;expression, its inherent inefficiencies are uncloaked as the sequence of expressions
;starts growing in number. For n expressions, n > 1, Alyssa's 'analyze-sequence' returns
;a 'lambda' expression that, when invoked at evaluation time, loops through the n
;execution procedures produced by analyzing the input expressions. This looping behavior,
;that the original variant avoids at evaluation time, absolutely hinders performance if a
;given sequence is to be evaluated more than once.
;---
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
           ((car procs) env)
           (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (lambda (env)
      (execute-sequence procs env))))

