
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.23.
;---
;A 'deque' ("double-ended queue") is a sequence in which items can be inserted and
;deleted at either the front or the rear. Operations on deques are the constructor
;'make-deque', the predicate 'empty-deque?', selectors 'front-deque' and 'rear-deque',
;mutators 'front-insert-deque!', 'rear-insert-deque!', 'front-delete-deque!', and
;'rear-delete-deque!'. Show how to represent deques using pairs, and give implementations
;of the operations. All operations should be accomplished in O(1) steps.
;------------------------------------------------------------------------------------------

;PROLOGUE
;---
;Two strategies are devised in the document. First, a 'deque' is implemented as a pair of
;pointers (front and rear) and each item added to the 'deque' is a list of two elements,
;the element itself and a pointer to the next item. Second, a 'deque' is impemented,
;again, as a pair of pointers, however, the items composing the data structure are lists
;not of two elements but rather of three, specifically, the element itself, a pointer to
;the previous item and a pointer to the next item. Pictoreal representations of the two
;'deque' implementations are provided below (in the order they were described). The first
;strategy is much simpler and saves us much trouble when defining all 'deque' mutators,
;with the exception of 'delete-rear-deque!'. In fact, the implementation of the latter
;procedure becomes impossible when such a strategy is followed: when deleting an item
;from the rear of the 'deque', we have no information on how we must set the rear pointer
;of the data structure. In truth, we may traverse the the whole 'deque', find the
;penultimate item (the item whose 'cddr' is '()) and set the rear pointer to such item.
;However, this violates a fundamental rule imposed by the authors: all operations must be
;performed in O(1) steps. Irrevocably, traversing the whole list to find its penultimate
;element requires O(n) steps. Hence, the more sophisticated (and convoluted) strategy of
;generating 'deques' whose items are triads. Whenever we want to remove an item from the
;rear of the 'deque', we must simply set the rear pointer of the data structure to the
;previous item of the item to be deleted and set the next item of the new rear of the
;'deque' to be '().
;---
;        +---+---+
; d ---> | | | --------------------------------------+
;        +-|-+---+                                   |
;          |                                         |
;          | front-ptr                               | rear-ptr
;          ↓                                         ↓
;        +---+---+     +---+---+     +---+---+     +---+---+
;        | | | ------> | | | ------> | | | ------> | | | / |
;        +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;          ↓             ↓             ↓             ↓
;         'a            'b            'c            'd
;---
;        +---+---+
; d ---> | | | ----------------------------------------------------------+
;        +-|-+---+                                                       |
;          |                                                             |
;          | fp    +-------------+   +-------------+   +-------------+   | rp
;          ↓       ↓             |   ↓             |   ↓             |   ↓
;        +---+---+---+     +---+-|-+---+     +---+-|-+---+     +---+-|-+---+
;        | | | / | ------> | | | | | ------> | | | | | ------> | | | | | / |
;        +-|-+---+---+     +-|-+---+---+     +-|-+---+---+     +-|-+---+---+
;          ↓                 ↓                 ↓                 ↓
;         'a                'b                'c                'd

;GENERAL PROCEDURES
;---
;Implementing the constructor, selectors and mutators for the front and rear pointers,
;and the predicate for testing whether a 'deque' is empty. These procedures are common to
;both strategies, since, 'deques' are invariably implemented as a set of two pointers.
;---
(define (make-deque) (cons '() '()))
;---
(define (front-ptr deque) (car deque))
;---
(define (rear-ptr deque) (cdr deque))
;---
(define (set-front-ptr! deque item)
  (set-car! deque item))
;---
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))
;---
(define (empty-deque? deque) (null? (front-ptr deque)))

;'DEQUE' SELECTORS AND MUTATORS -> 1st STRATEGY
;---
(define (front-deque-1 deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))
;---
(define (rear-deque-1 deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))
;---
(define (front-insert-deque-1! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! new-pair (front-ptr deque))
           (set-front-ptr! deque new-pair)))
    deque))
;---
(define (rear-insert-deque-1! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)))
    deque))
;---
(define (front-delete-deque-1! deque)
  (if (empty-deque? deque)
      (error "FRONT-DELETE! called with an empty deque" deque)
      (begin (set-front-ptr! deque (cdr (front-ptr deque)))
             deque)))
;---
(define (rear-delete-deque-1! deque)
  (if (empty-deque? deque)
      (error "REAR-DELETE! called with an empty deque" deque)
      (error "REAR-DELETE!: cannot set the rear pointer" deque)))

;'DEQUE' SELECTORS AND MUTATORS -> 2nd STRATEGY
;---
;For reasons of legibility, in addition to the selectors and mutators, a constructor,
;selectors and mutators for items that are to compose the data structure are provided.
;---

(define (make-item elem prev next)
  (lambda (m) (m elem
                 prev
                 next
                 (lambda (n) (set! elem n))
                 (lambda (n) (set! prev n))
                 (lambda (n) (set! next n)))))
;---
(define (element item)
  (item (lambda (e p n se sp sn) e)))
;---
(define (previous-item item)
  (item (lambda (e p n se sp sn) p)))
;---
(define (next-item item)
  (item (lambda (e p n se sp sn) n)))
;---
(define (set-element! item new-val)
  (item (lambda (e p n se sp sn) (se new-val))))
;---
(define (set-previous-item! item new-val)
  (item (lambda (e p n se sp sn) (sp new-val))))
;---
(define (set-next-item! item new-val)
  (item (lambda (e p n se sp sn) (sn new-val))))
;---
(define (front-deque-2 deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (element (front-ptr deque))))
;---
(define (rear-deque-2 deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (element (rear-ptr deque))))
;---
(define (front-insert-deque-2! deque elem)
  (let ((new-item (make-item elem '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item))
          (else
             (set-next-item! new-item (front-ptr deque))
             (set-previous-item! (front-ptr deque) new-item)
             (set-front-ptr! deque new-item)))
    deque))
;---
(define (rear-insert-deque-2! deque elem)
  (let ((new-item (make-item elem '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item))
          (else
           (set-next-item! (rear-ptr deque) new-item)
           (set-previous-item! new-item (rear-ptr deque))
           (set-rear-ptr! deque new-item)))
    deque))
;---
(define (front-delete-deque-2! deque)
  (if (empty-deque? deque)
      (error "FRONT-DELETE! called with an empty deque" deque)
      (begin (set-front-ptr! deque (next-item (front-ptr deque)))
             (if (not (null? (front-ptr deque)))
                 (set-previous-item! (front-ptr deque) '())
                 (set-rear-ptr! deque '()))
             deque)))
;---
(define (rear-delete-deque-2! deque)
  (if (empty-deque? deque)
      (error "REAR-DELETE! called with an empty deque" deque)
      (begin (set-rear-ptr! deque (previous-item (rear-ptr deque)))
             (if (not (null? (rear-ptr deque)))
                 (set-next-item! (rear-ptr deque) '())
                 (set-front-ptr! deque '()))
             deque)))

;TEST 2nd IMPLEMENTATION OF 'DEQUES'
;---
(define (print-deque deque)
  ;---
  (define (loop-deque d)
    (display " <-> ")
    (if (null? d)
        (newline)
        (begin (display (element d))
               (loop-deque (next-item d)))))
  ;---
  (loop-deque (front-ptr deque)))
;---
(let ((d1 (make-deque)))
  (print-deque d1) ; <-> 
  (print-deque (front-insert-deque-2! d1 1)) ; <-> 1 <-> 
  (print-deque (front-insert-deque-2! d1 2)) ; <-> 2 <-> 1 <-> 
  (print-deque (front-insert-deque-2! d1 3)) ; <-> 3 <-> 2 <-> 1 <-> 
  (print-deque (rear-insert-deque-2! d1 4)) ; <-> 3 <-> 2 <-> 1 <-> 4 <-> 
  (print-deque (front-delete-deque-2! d1)) ; <-> 2 <-> 1 <-> 4 <-> 
  (print-deque (rear-insert-deque-2! d1 5)) ; <-> 2 <-> 1 <-> 4 <-> 5 <-> 
  (print-deque (front-delete-deque-2! d1)) ; <-> 1 <-> 4 <-> 5 <-> 
  (print-deque (front-delete-deque-2! d1)) ; <-> 4 <-> 5 <-> 
  (print-deque (rear-delete-deque-2! d1)) ; <-> 4 <-> 
  (print-deque (front-delete-deque-2! d1))) ; <-> 

