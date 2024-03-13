
#lang sicp


;------------------------------------------------------------------------------------------
;ONE-DIMESIONAL TABLES
;------------------------------------------------------------------------------------------
;In one-dimensional tables, each value is identified by a single key. The table is
;represented as a list of records, whose head is a dummy record identifying the start of
;the table (usually denoted by '*table*).

;example of box-and-pointer diagram for a one-dimensional table
;---
;            +---+---+     +---+---+     +---+---+     +---+---+
; table ---> | | | ------> | | | ------> | | | ------> | | | / |
;            +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;              ↓             ↓             ↓             ↓
;           *table*        +---+---+     +---+---+     +---+---+
;                          | | | | |     | | | | |     | | | | |
;                          +-|-+-|-+     +-|-+-|-+     +-|-+-|-+
;                            ↓   ↓         ↓   ↓         ↓   ↓
;                           'a   1        'b   2        'c   3

;'assoc' (helper procedure that finds the record whose associated key is the key it takes
;as input; footnote 24 -> "Because 'assoc' uses 'equal?', it can recognize keys that are
;symbols, numbers, or list structure.")
;---
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;'lookup-1d' (looks up the value stored under the key it takes as input)
;---
(define (lookup-1d key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

;'insert-1d!' (inserts a new pair (key, value) in the table; if the key is already part
;of the table, simply modify the corresponding value to 'value', otherwise, set the 'cdr'
;of the table to be a pair whose 'car' is a pair (key, value) and whose 'cdr' is the old
;'cdr' of the table)
;---
(define (insert-1d! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value)
                              (cdr table)))))
  'ok)

;'make-table' (constructing a new table)
;---
(define (make-table) (list '*table*))


;------------------------------------------------------------------------------------------
;TWO-DIMENSIONAL TABLES
;------------------------------------------------------------------------------------------
;In two-dimensional tables, each value is associated to a pair of keys. Here, the first
;record of the table (excluding the dummy record) is a pair of subtables.

;example of a two-dimensional table
;---
;            +---+---+     +---+---+     +---+---+
; table ---> | | | ------> | | | ------> | | | / |
;            +-|-+---+     +-|-+---+     +-|-+---+
;              ↓             |             ↓
;           *table*          |           +---+---+     +---+---+     +---+---+
;                            |           | | | ------> | | | ------> | | | / |
;                            |           +-|-+---+     +-|-+---+     +-|-+---+
;                            |             ↓             ↓             ↓
;                            |        *subtable1*      +---+---+     +---+---+
;                            |                         | | | | |     | | | | |
;                            |                         +-|-+-|-+     +-|-+-|-+
;                            |                           ↓   ↓         ↓   ↓
;                            |                          'a1  1        'b1  2
;                            ↓
;                          +---+---+     +---+---+     +---+---+     +---+---+
;                          | | | ------> | | | ------> | | | ------> | | | / |
;                          +-|-+---+     +-|-+---+     +-|-+---+     +-|-+---+
;                            ↓             ↓             ↓             ↓
;                       *subtable2*      +---+---+     +---+---+     +---+---+
;                                        | | | | |     | | | | |     | | | | |
;                                        +-|-+-|-+     +-|-+-|-+     +-|-+-|-+
;                                          ↓   ↓         ↓   ↓         ↓   ↓
;                                         'a2  3        'b2  4        'c   5

;'lookup-2d'
;---
(define (lookup-2d key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

;'insert-2d!'
;---
(define (insert-2d! key-1 key-2 value table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value)
                                       (cdr subtable)))))
        (set-cdr! table (cons (list key-1
                                    (cons key-2 value))
                              (cdr table))))))


;------------------------------------------------------------------------------------------
;GENERATING TWO-DIMENSIONAL LOCAL TABLES ['put' and 'get']
;------------------------------------------------------------------------------------------
;In chapter 2, we have made extensive use of the procedures 'put' and 'get' to operate on
;tables. Here, we implement these procedures. They both exploit of a predefined table
;going by the name 'operations-table'. Such a table corresponds to a procedure that
;accepts a message and returns the appropriate procedure for further use. Moreover, the
;table object maintains local state, in this case, 'local-table' which is initialized to
;the list '(*table*).

;'make-table'
;---
(define (make-local-table)
  ;---
  (let ((local-table (list '*table*)))
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
            ((eq? m 'print) local-table)
            (else (error "Unknown operation: TABLE" m))))))

;finally, defining 'put', 'get' and 'print'
;---
(define ops-table (make-local-table))
(define get (ops-table 'lookup))
(define put (ops-table 'insert!))
(define (print) (ops-table 'print))

;testing local tables
;---
(put 'biology 'ecology 101)
(put 'biology 'physiology 102)
(put 'biology 'genetics 103)
;---
(display "genetics ") (get 'biology 'genetics)
;---
(print)
;---
(put 'math 'calculus 201)
(put 'math 'algebra 202)
(put 'math 'number-theory 203)
;---
(display "algebra ") (get 'math 'algebra)
;---
(print)

