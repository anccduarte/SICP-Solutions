
#lang sicp


;------------------------------------------------------------------------------------------
;CONNECTORS
;------------------------------------------------------------------------------------------

;'make-connector' -> constructing a connector object
;[note that 'me' is a dispatch procedure representing the connector - see last procedure
;for its definition]
;---
(define (make-connector)
  ;---
  (let ((value false) (informant false) (constraints '()))
    ;---
    (define (set-my-value! new-value setter)
      (cond ((not (has-value? me))
             (set! value new-value)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value new-value))
             (error "Contradiction" (list value new-value)))
            (else
             'ignored)))
    ;---
    (define (forget! retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    ;---
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    ;---
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value!)
            ((eq? request 'forget!) forget!)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))
    ;---
    me))

;basic operations on connectors
;[note that 'has-value?' is exploited in the constructor itself]
;---
(define (has-value? connector)
  (connector 'has-value?))
;---
(define (get-value connector)
  (connector 'value))
;---
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
;---
(define (forget-value! connector retractor)
  ((connector 'forget!) retractor))
;---
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;'for-each-except' -> calls the procedure it takes as second argument for all the
;constraints it takes as third argument except if a constraint is equal to the constraint
;specified by the first argument
;---
(define (for-each-except exception proc lst)
  ;---
  (define (loop items)
    (cond ((null? items)
           'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else
           (proc (car items))
           (loop (cdr items)))))
  ;---
  (loop lst))


;------------------------------------------------------------------------------------------
;CONSTRAINTS
;------------------------------------------------------------------------------------------

;syntactic sugar for processing constraint requests
;---
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
;---
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;'adder'
;[note that, as in connectors (above), 'me' is a dispatch procedure that represents the
;constraint]
;---
(define (adder a1 a2 sum)
  ;---
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  ;---
  (define (process-forget-value)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (forget-value! sum me)
    (process-new-value))
  ;---
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  ;---
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;'multiplier'
;---
(define (multiplier m1 m2 product)
  ;---
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  ;---
  (define (process-forget-value)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (forget-value! product me)
    (process-new-value))
  ;---
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  ;---
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;'squarer'
;---
(define (squarer a b)
  ;---
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b) 0)
               (error "square less than 0: SQUARER" (get-value b))
               (set-value! a (sqrt (get-value b)) me)))
          ((has-value? a)
           (set-value! b (* (get-value a) (get-value a)) me))))
  ;---
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  ;---
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  ;---
  (connect a me)
  (connect b me)
  me)

;'constant'
;---
(define (constant value connector)
  ;---
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  ;---
  (connect connector me)
  (set-value! connector value me)
  me)

;'probe'
;---
(define (probe name connector)
  ;---
  (define (print-probe value)
    (display "Probe: ") (display name)
    (display " = ") (display value) (newline))
  ;---
  (define (process-new-value)
    (print-probe (get-value connector)))
  ;---
  (define (process-forget-value)
    (print-probe "?"))
  ;---
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  ;---
  (connect connector me)
  me)


;------------------------------------------------------------------------------------------
;USING THE CONSTRAINT SYSTEM
;------------------------------------------------------------------------------------------

;pictoreal representation of the constraint system (celsius-fahrenheit)
;---
;9C = 5(32-F)
;---
;       +--------+          +--------+    v     +--------+
; C --- | m1     |    u     |     m1 | -------- | a1     |
;       |    * p | -------- | p *    |          |    + s | --- F
;   +-- | m2     |          |     m2 | --+  +-- | a2     |
;   |   +--------+          +--------+   |  |   +--------+
;   |                                    |  |
;   +------ 9                    5 ------+  +------ 32
;      w                              x        y

;bidirectional celsius-fahrenheit converter
;---
(define (celsius-fahrenheit-converter c f)
  (let ((x (make-connector))
        (y (make-connector))
        (w (make-connector))
        (u (make-connector))
        (v (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;test converter
;---
(let ((C (make-connector))
      (F (make-connector)))
  ;---
  (celsius-fahrenheit-converter C F)
  ;---
  (probe "Celsius temp" C)
  (probe "Fahrenheit temp" F)
  ;---
  (set-value! C 25 'user)
  ;(set-value! F 212 'user)
  (display "---") (newline)
  (forget-value! C 'user)
  (display "---") (newline)
  (set-value! F 212 'user)
  (display ""))
