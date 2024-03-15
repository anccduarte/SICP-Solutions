
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.25.
;---
;Generalizing one- and two-dimensional tables, show how to implement a table in which
;values are stored under an arbitrary number of keys and different values may be stored
;under different numbers of keys. The 'lookup' and 'insert!' procedures should take as
;input a list of keys used to access the table.
;------------------------------------------------------------------------------------------

;'table?' -> predicate for identifying tables
;---
(define (table? table)
  (and (list? table)
       (> (length table) 0)))

;'make-table'
;---
(define (make-table) (list '*table*))

;'assoc'
;---
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;'lookup' -> accepts as first argument a list of keys (as soon as we have a record and an
;empty set of keys, we return the contents of the record no matter if the record is an
;entry - i.e., a scheme pair - or a subtable)
;---
(define (lookup keys table)
  ;---
  (define (lookup-iter keys table)
    (let ((record (assoc (car keys) (cdr table))))
      (if record
          (if (null? (cdr keys))
              (cdr record)
              (lookup-iter (cdr keys) (if (table? record)
                                          record
                                          '(*quit*))))
          #f)))
  ;---
  (if (or (null? keys) (not (table? table)))
      (error "LOOKUP: Bad argument(s) - KEYS - TABLE"
             (list keys table))
      (lookup-iter keys table)))

;'insert!'
;---
(define (insert! keys value table)
  ;---
  (define (insert!-iter keys table)
    ;---
    (let* ((head-key (car keys))
           (rest-keys (cdr keys))
           (record (assoc head-key (cdr table))))
      ;---
      (if record
          ;---
          (if (null? rest-keys)
              ;---
              (if (not (table? record))
                  (set-cdr! record value)
                  (set-cdr! record (cons (head-key value)
                                         (cdr record))))
              ;---
              (if (not (table? record))
                  (begin (set-cdr! record '())
                         (insert!-iter keys table))
                  (insert!-iter rest-keys record)))
          ;---
          (if (null? rest-keys)
              (set-cdr! table (cons (cons head-key value)
                                    (cdr table)))
              (begin (set-cdr! table (cons (list head-key)
                                           (cdr table)))
                     (insert!-iter keys table))))))
  ;---
  (if (or (null? keys) (not (table? table)))
      (error "INSERT!: Bad argument(s) - KEYS - TABLE"
             (list keys table))
      (insert!-iter keys table)))

;test
;---
(define (print-table table)
  (display "table -> ")
  (display table) (newline))
;---
(define (print-lookup keys table)
  (display keys) (display " -> ")
  (display (lookup keys table)) (newline))
;---
(let ((table (make-table)))
  (insert! '(a) 1 table)
  (insert! '(*subtable* b) 2 table)
  (insert! '(*subtable* c) 3 table)
  (insert! '(*subtable* d) 4 table)
  (print-table table)
  (print-lookup '(*subtable*) table)
  (print-lookup '(*subtable* b) table)
  (insert! '(*subtable* b e) 5 table)
  (print-table table)
  (print-lookup '(*subtable* b e) table)
  (print-lookup '(*subtable* b a) table))

