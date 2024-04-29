
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.47.
;---
;A semaphore (of size 'n') is a generalization of a mutex. Like a mutex, a semaphore
;supports acquire and release operations, but it is more general in that up to 'n'
;processes can acquire it concurrently. Additional processes that attempt to acquire the
;semaphore must wait for release operations. Give implementations of semaphores
;---
;(a) in terms of mutexes
;---
;(b) in terms of atomic test-and-set! operations.
;------------------------------------------------------------------------------------------

;Mutexes
;---
(define (clear! cell)
  (set-car! cell false))
;---
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))
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

;Semaphores in terms of mutexes
;[To acquire the semaphore, there must be at least an empty slot. If there is, we must
;simply increase the counter of reserved slots and attempt to acquire the mutex. If there
;is not, we retry acquiring the semaphore. Releasing the semaphore is easier: decrement
;the counter of reserved slots and release the mutex (note that releasing the semaphore
;always implies releasing the mutex, since the former signals that the process which
;acquired the latter has finished).]
;---
(define (make-semaphore n)
  ;---
  (let ((mutex (make-mutex))
        (reserved 0))
    ;---
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (< reserved n)
                 (begin (set! reserved (+ reserved 1))
                        (mutex 'acquire))
                 (the-semaphore 'acquire)))
            ((eq? m 'release)
             (set! reserved (- reserved 1))
             (mutex 'release))))
    ;---
    the-semaphore))

