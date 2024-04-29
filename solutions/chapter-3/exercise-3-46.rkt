
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.46.
;---
;Suppose that we implement 'test-and-set!' using an ordinary procedure as shown in the
;text, without attempting to make the operation atomic. Draw a timing diagram like the
;one in Figure 3.29 to demonstrate how the mutex implementation can fail by allowing two
;processes to acquire the mutex at the same time.
;------------------------------------------------------------------------------------------

;Implemeting serializers in terms of a mutex
;---
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
;---
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))
;---
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
;---
(define (clear! cell)
  (set-car! cell false))

;Demonstrate how the mutex implementation can fail by allowing two processes to acquire
;the mutex at the same time.
;---
;Suppose we have two processes, P1 and P2, trying to acquire a mutex whose cell is set to
;false. If atomicity is not guaranteed by 'test-and-set!', the following may happen:
;---
;1. P1 tries to acquire the mutex [(mutex 'acquire)], and finds the mutex to be available
;   [(test-and-set! cell); (if (car cell))]
;---
;2. P2 attempts to acquire the mutex [(mutex 'acquire)], also finding it to be set to
;   false [(test-and-set! cell); (if (car cell))] since P1's 'test-and-set!' has not yet
;   finished executing
;---
;3. P1's 'test-and-set!' sets the mutex to true and returns false
;---
;4. P2's 'test-and-set!' sets the mutex to true and returns false
;---
;Athough the mutex was correctly set to true (blocking any other processes depending on
;the state of the mutex to be executed), P1 and P2 end up acquiring the mutex at once,
;potentially causing trouble if both try to simultaneously access and mutate the shared
;resource [see Figure 3.29.]. The atomicity provided by the variant of 'test-and-set!'
;prevents such an issue, as it always fully executes before any other call to the
;procedure is effected. In other words, the execution of 'test-and-set!' by two or more
;processes may not be interleaved: in our example, the order of execution would, in such
;circumstances, be either 1->3->2->4 or 2->4->1->3.

