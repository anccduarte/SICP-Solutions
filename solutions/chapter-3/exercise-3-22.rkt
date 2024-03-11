
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.22.
;---
;Instead of representing a queue as a pair of pointers, we can build a queue as a
;procedure with local state. The local state will consist of pointers to the beginning
;and the end of an ordinary list. Thus, the 'make-queue' procedure will have the form
;---
;(define (make-queue)
;  (let ((front-ptr ...)
;        (rear-ptr ...))
;    <definitions of internal procedures>
;    (define (dispatch m) ...)
;    dispatch))
;---
;Complete the definition of 'make-queue' and provide implementations of the queue
;operations using this representation.
;------------------------------------------------------------------------------------------

;important remark
;---
;note that this implementation does not affect the remaining procedures, that is, a solid
;abstraction barrier was erected between the procedures that represent queues and the
;procedures that use and manipulate queues; the procedures 'empty-queue?', 'front-queue',
;'insert-queue!' and 'delete-queue!' do not "care" on how queues are represented; coming
;up with a representaion for queues is "George's job"; the higher level procedures take
;the construction of 'make-queue', 'front-ptr', 'rear-ptr', 'set-front-ptr!' and
;'set-rear-ptr!' for granted, and are solely concerned about what they return

;alternative implementation of 'make-queue'
;---
(define (make-queue)
  ;---
  (let ((front-ptr '())
        (rear-ptr '()))
    ;---
    (define (set-front! item) (set! front-ptr item))
    (define (set-rear! item) (set! rear-ptr item))
    ;---
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front!)
            ((eq? m 'set-rear-ptr!) set-rear!)
            (else
             (error "MAKE-QUEUE: unknown request" m))))
    ;---
    dispatch))

;defining new selectors
;---
(define (front-ptr queue) (queue 'front-ptr))
;---
(define (rear-ptr queue) (queue 'rear-ptr))

;defining new mutators
;---
(define (set-front-ptr! queue item)
  ((queue 'set-front-ptr!) item))
;---
(define (set-rear-ptr! queue item)
  ((queue 'set-rear-ptr!) item))

