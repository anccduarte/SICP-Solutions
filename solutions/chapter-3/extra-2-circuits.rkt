
#lang sicp


;------------------------------------------------------------------------------------------
;WIRES
;------------------------------------------------------------------------------------------

;'call-each' -> calling wire action procedures
;---
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

;'make-wire' -> constructing wire objects
;---
(define (make-wire)
  ;---
  (let ((signal-value 0) (action-procedures '()))
    ;---
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))))
    ;---
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc
                                    action-procedures))
      (proc))
    ;---
    (lambda (m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)))))

;selectors and mutators for wires
;[footnote 27 -> "These procedures are simply syntactic sugar that allow us to use
;ordinary procedural syntax to access the local procedures of objects. It is striking
;that we can interchange the role of "procedures" and "data" in such a simple way. For
;example, if we write '(wire 'get-signal)' we think of wire as a procedure that is called
;with the message 'get-signal' as input. Alternatively, writing '(get-signal wire)'
;encourages us to think of wire as a data object that is the input to a procedure
;'get-signal'. The truth of the matter is that, in a language in which we can deal with
;procedures as objects, there is no fundamental difference between "procedures" and
;"data", and we can choose our syntactic sugar to allow us to program in whatever style
;we choose."]
;---
(define (get-signal wire) (wire 'get-signal))
;---
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
;---
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


;------------------------------------------------------------------------------------------
;QUEUES
;[the data structure is exploited in the implementation of agendas]
;------------------------------------------------------------------------------------------

;'make-queue'
;---
(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (lambda (m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!)
             (lambda (n) (set! front-ptr n)))
            ((eq? m 'set-rear-ptr!)
             (lambda (n) (set! rear-ptr n)))
            (else
             (error "Unknown request" m))))))

;'empty-queue?'
;---
(define (empty-queue? queue)
  (null? (front-ptr queue)))

;selectors and mutators for queues
;---
(define (front-ptr queue) (queue 'front-ptr))
;---
(define (rear-ptr queue) (queue 'rear-ptr))
;---
(define (set-front-ptr! queue new-value)
  ((queue 'set-front-ptr!) new-value))
;---
(define (set-rear-ptr! queue new-value)
  ((queue 'set-rear-ptr!) new-value))

;'front-queue'
;---
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;'insert-queue!'
;---
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair))))
  queue)

;'delete-queue!'
;---
(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE called with an empty queue" queue)
      (begin (set-front-ptr! queue (cdr (front-ptr queue)))
             queue)))


;------------------------------------------------------------------------------------------
;AGENDAS
;["The agenda is made up of time segments. Each time segment is a pair consisting of a
;number (the time) and a queue that holds the procedures that are scheduled to be run
;during that time segment."]
;------------------------------------------------------------------------------------------

;time segments
;---
(define (make-time-segment t q) (cons t q))
;---
(define (segment-time s) (car s))
;---
(define (segment-queue s) (cdr s))

;agendas -> basic operations
;---
(define (make-agenda) (list 0))
;---
(define (current-time agenda) (car agenda))
;---
(define (set-current-time! agenda time)
  (set-car! agenda time))
;---
(define (segments agenda) (cdr agenda))
;---
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
;---
(define (first-segment agenda) (car (segments agenda)))
;---
(define (rest-segments agenda) (cdr (segments agenda)))
;---
(define (empty-agenda? agenda) (null? (segments agenda)))

;'add-to-agenda!' -> add action to an agenda at a specified time
;---
(define (add-to-agenda! time action agenda)
  ;---
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  ;---
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  ;---
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest-segments (cdr segments)))
          (if (belongs-before? rest-segments)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              rest-segments))
              (add-to-segments! rest-segments)))))
  ;---
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

;'remove-first-agenda-item!'
;[footnote 29 -> "Observe that the 'if' expression in this procedure has no <alternative>
;expression. Such a «one-armed 'if' statement» is used to decide whether to do something,
;rather than to select between two expressions. An 'if' expression returns an unspecified
;value if the predicate is false and there is no <alternative>."]
;---
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

;'first-agenda-item'
;["Whenever we extract an item, we also update the current time. In this way, the current
;time will always be the time of the action most recently processed. Storing this time at
;the head of the agenda ensures that it will still be available even if the associated
;time segment has been deleted."]
;---
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


;------------------------------------------------------------------------------------------
;PRIMITIVE OPERATIONS ON WIRES
;['inverter', 'or-gate' and 'and-gate']
;------------------------------------------------------------------------------------------

;'after-delay' -> adding action procedures to an agenda object
;---
(define (after-delay delay-time action)
  (add-to-agenda! (+ delay-time (current-time the-agenda))
                  action
                  the-agenda))

;'logical-not', 'inverter-delay' and 'inverter'
;---
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
;---
(define inverter-delay 2)
;---
(define (inverter input output)
  ;---
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  ;---
  (add-action! input invert-input))

;'logical-or', 'or-gate-delay' and 'or-gate'
;---
(define (logical-or s1 s2)
  (if (and (or (= s1 0) (= s1 1))
           (or (= s2 0) (= s2 1)))
      (if (and (= s1 0) (= s2 0)) 0 1)
      (error "Invalid signal(s)" (list s1 s2))))
;---
(define or-gate-delay 5)
;---
(define (or-gate a1 a2 output)
  ;---
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  ;---
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

;'logical-and', 'and-gate-delay' and 'and-gate'
;---
(define (logical-and s1 s2)
  (if (and (or (= s1 0) (= s1 1))
           (or (= s2 0) (= s2 1)))
      (if (and (= s1 1) (= s2 1)) 1 0)
      (error "Invalid signal(s)" (list s1 s2))))
;---
(define and-gate-delay 3)
;---
(define (and-gate a1 a2 output)
  ;---
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  ;---
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))


;------------------------------------------------------------------------------------------
;MEANS OF ABTRACTION
;['half-adder', 'full-adder' and 'ripple-carry-adder']
;------------------------------------------------------------------------------------------

;'half-adder'
;---
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;'full-adder'
;---
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;'ripple-carry-adder'
;---
(define (ripple-carry-adder a b c-in s)
  (if (not (null? a))
      'ok
      (let ((c-out (make-wire)))
        (full-adder (car a) (car b) c-in (car s) c-out)
        (ripple-carry-adder (cdr a) (cdr b) c-out (cdr s)))))


;------------------------------------------------------------------------------------------
;SIMULATION
;------------------------------------------------------------------------------------------

;'the-agenda' -> queue of action procedures
;---
(define the-agenda (make-agenda))

;'propagate' -> executes all action procedures in 'the-agenda'
;[remember that the first action is given by the 'cdr' of the first item]
;---
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-action (first-agenda-item the-agenda)))
        (first-action)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;'probe'
;---
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

;test 1 -> setting wires and probing
;---
(display "TEST 1") (newline)
;---
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
;---
(probe 'sum sum)
(probe 'carry carry)

;test 2 -> constructing circuit and setting input signals
;---
(newline) (display "TEST 2") (newline)
;---
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)

