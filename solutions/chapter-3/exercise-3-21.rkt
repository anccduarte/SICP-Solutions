
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.21.
;---
;Ben Bitdiddle decides to test the queue implementation described above. He types in the
;procedures to the Lisp interpreter and proceeds to try them out:
;---
;(define q1 (make-queue))
;(insert-queue! q1 'a) -> ((a) a)
;(insert-queue! q1 'b) -> ((a b) b)
;(delete-queue! q1) -> ((b) b)
;(delete-queue! q1) -> (() b)
;---
;"It's all wrong!" he complains. "The interpreter's response shows that the last item is
;inserted into the queue twice. And when I delete both items, the second 'b' is still
;there, so the queue isn't empty, even though it's supposed to be." Eva Lu Ator suggests
;that Ben has misunderstood what is happening. "It's not that the items are going into
;the queue twice", she explains. "It's just that the standard Lisp printer doesn't know
;how to make sense of the queue representation. If you want to see the queue printed
;correctly, you'll have to define your own print procedure for queues." Explain what Eva
;Lu is talking about. In particular, show why Ben's examples produce the printed results
;that they do. Define a procedure 'print-queue' that takes a queue as input and prints
;the sequence of items in the queue.
;------------------------------------------------------------------------------------------

;on representing queues
;---
;"(...) this representation [an ordinary list] is inefficient, because in order to insert
;an item we must scan the list until we reach the end. Since the only method we have for
;scanning a list is by successive 'cdr' operations, this scanning requires O(n) steps for
;a list of 'n' items. A simple modification to the list representation overcomes this
;disadvantage by allowing the queue operations to be implemented so that they require
;O(1) steps; that is, so that the number of steps needed is independent of the length of
;the queue. [It's important to mention that 'append' - the procedure used to add an item
;to the end of an ordinary immutable list - does exactly what is previously stated: in
;order to "add" an element to a list, it must first scan the entire original list to look
;for its last element.] (...) The modification that avoids the drawback is to represent
;the queue as a list, together with an additional pointer that indicates the final pair
;in the list. That way, when we go to insert an item, we can consult the rear pointer and
;so avoid scanning the list."

;pictoreal representation of a queue
;---
;        +---+---+
; q ---> | | | --------------------------------------+
;        +-|-+---+                                   |
;          |                                         |
;          | front-ptr                               | rear-ptr
;          ↓                                         ↓
;        +---+---+     +---+---+     +---+---+     +---+---+
;        | | | ------> | | | ------> | | | ------> | | | / |
;        +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;          ↓             ↓             ↓             ↓
;         'a            'b            'c            'd

;implementing queues
;---
;(a queue is a pair of two "mutable" pointers that indicate the front and the rear of the
;queue; to make a queue, we simply construct a pair consisting of two empty lists; a
;queue is considered to be empty if the front pointer is the empty list; to select the
;first item of a queue, we simply return the 'car' of the front pointer; when inserting
;an item in a queue, two scenarios emerge: 1. the queue is empty - in this case, we
;simply set the front and rear pointers of the queue to the newly constructed pair; the
;queue isn't empty - in this case, we set the 'cdr' of the pair pointed to by the rear
;pointer to be the new pair, and update the rear pointer to be the new pair; deleting an
;item from the queue is simpler: we simply set the front pointer of the queue to be 'cdr'
;of the item pointed to by the front pointer)
;---
;"If the first item is the final item in the queue, the front pointer will be the empty
;list after the deletion, which will mark the queue as empty; we needn’t worry about
;updating the rear pointer, which will still point to the deleted item, because
;'empty-queue?' looks only at the front pointer."
;---
(define (front-ptr queue)
  (car queue))
;---
(define (rear-ptr queue)
  (cdr queue))
;---
(define (set-front-ptr! queue item)
  (set-car! queue item))
;---
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
;---
(define (empty-queue? queue)
  (null? (front-ptr queue)))
;---
(define (make-queue)
  (cons '() '()))
;---
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
;---
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)))
    queue))
;---
(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE! called with an empty queue" queue)
      (begin (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))

;SOLUTION to exercise
;---
;the explanation on the internal representation of queues is thoroughly explored in the
;notes above; summarily, so that the insertion of items in the queue is performed in O(1)
;steps, the data structure must keep information (a pointer) about the rear of the queue;
;since the rear of the queue corresponds to the last item of the queue itself, it pops up
;twice when the Lisp interpreter prints the data structure; the behavior regarding the
;deletion of an item from a queue consisting of a single element is exposed in footnote
;22; to amend such an "issue", we may devise a procedure that solely looks at the front
;pointer of the queue
;---
(define (print-queue queue)
  ;---
  (define (print-iter q)
    (display " <- ")
    (if (null? q)
        (newline)
        (begin (display (car q))
               (print-iter (cdr q)))))
  ;---
  (print-iter (front-ptr queue)))

;test 'print-queue'
;---
(let ((q1 (make-queue)))
  (print-queue (insert-queue! q1 'a))
  (print-queue (insert-queue! q1 'b))
  (print-queue (delete-queue! q1))
  (print-queue (delete-queue! q1)))

