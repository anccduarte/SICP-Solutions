
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.24.
;---
;In the table implementations above, the keys are tested for equality using 'equal?'
;(called by 'assoc'). This is not always the appropriate test. For instance, we might
;have a table with numeric keys in which we don't need an exact match to the number we're
;looking up, but only a number within some tolerance of it. Design a table constructor
;'make-table' that takes as an argument a 'same-key?' procedure that will be used to test
;"equality" of keys. 'make-table' should return a 'dispatch' procedure that can be used
;to access appropriate 'lookup' and 'insert!' procedures for a local table.
;------------------------------------------------------------------------------------------

;redefining 'make-table'
;---
(define (make-local-table same-key?)
  ;---
  (let ((local-table (list '*table*)))
    ;---
    (define (assoc key tab)
      (cond ((null? tab) #f)
            ((same-key? key (caar tab)) (car tab))
            (else (assoc key (cdr tab)))))
    ;---
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    ;---
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1
                                              (cons key-2 value))
                                        (cdr local-table))))))
    ;---
    (lambda (m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) (lambda () local-table))
            (else (error "Unknown operation: TABLE" m))))))

;test new implementation
;---
(let* ((ops-table (make-local-table eq?))
       (get (ops-table 'lookup))
       (put (ops-table 'insert!))
       (print (ops-table 'print)))
  (put 'x 'y 1) (put 'x 'w 2) (put 'x 'z 3)
  (put 'y 'x 4) (put 'y 'w 5) (put 'y 'z 6)
  (display (print)) (newline)
  (display (get 'x 'z)) (newline)
  (display (get 'y 'w)) (newline))
;---
(display "---") (newline)
;---
(let* ((ops-table (make-local-table
                   (lambda (a b) (< (abs (- a b)) 0.0001))))
       (get (ops-table 'lookup))
       (put (ops-table 'insert!))
       (print (ops-table 'print)))
  (put 1 2 'a) (put 1 3 'b) (put 1 4 'c)
  (display (print)) (newline)
  (put 1.00005 2 'd) (put 1 3.01 'e) (put 1 4.005 'f)
  (display (print)) (newline)
  (display (get 1 2)) (newline))

