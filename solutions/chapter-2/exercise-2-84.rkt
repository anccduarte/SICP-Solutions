
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.84.
;---
;Using the 'raise' operation of Exercise 2.83, modify the 'apply-generic' procedure so
;that it coerces its arguments to have the same type by the method of successive raising,
;as discussed in this section. You will need to devise a way to test which of two types
;is higher in the tower. Do this in a manner that is "compatible" with the rest of the
;system and will not lead to problems in adding new levels to the tower.
;------------------------------------------------------------------------------------------

;'type-tag' and 'contents'
;---
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "bad datum")))
;---
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad datum")))

;'repeated' (repeated application of procedure; note that it exclusively works when
;applied to procedures of a single argument)
;---
(define (repeated proc n)
  (if (= n 1)
      proc
      (lambda (x)
        (proc ((repeated proc (- n 1)) x)))))

;'raise' (from Exercise 2.83.)
;---
(define tower-of-types '(integer rational real complex))
;---
(define (memq elem lst)
  (cond ((null? lst)
         #f)
        ((eq? elem (car lst))
         lst)
        (else
         (memq elem (cdr lst)))))
;---
(define (raise arg)
  (let ((subtower (memq (type-tag arg) tower-of-types)))
    (if subtower
        (if (null? (cdr subtower))
            arg
            (let ((raiser (get-coercion (car subtower)
                                        (cadr subtower))))
              (if raiser
                  (raiser (contents arg))
                  (error "no coercion procedure for type"))))
        (error "type not in tower of types"))))

;It is assumed that 'apply-generic' only works for operations of two arguments, whenever
;no procedure is found for 'op' and 'args'. This reflects the implementation proposed by
;the authors. The idea is to "associate" an integer to each type that specifies their
;position in the tower. To know which type is higher in the tower, simply compare their
;positions.
;---
(define (apply-generic op . args)
  ;---
  (define (get-position type tower)
    (define (iter-tower tower pos)
      (cond ((null? tower)
             (error "type not in tower of types"))
            ((eq? type (car tower))
             pos)
            (else
             (iter-tower (cdr tower) (+ pos 1)))))
    (iter-tower tower 1))
  ;---
  (define (coerce-lower args type-args)
    (define (get-diff)
      (let ((pos1 (get-position (car type-args) tower-of-types))
            (pos2 (get-position (cadr type-args) tower-of-types)))
        (- pos1 pos2))
    (let ((diff-types (get-diff)))
      (cond ((= diff-types 0)
             (error "no operation for types"))
            ((< diff-types 0)
             (list ((repeated raise (abs diff)) (car args))
                   (cadr args)))
            ((> diff-types 0)
             (list (car args)
                   ((repeated raise diff) (cadr args))))))))
  ;---
  (define (apply-generic-main)
    (let* ((type-args (map type-tag args))
           (proc1 (get op type-args)))
      (if proc1
          (apply proc1 (map contents args))
          (if (= (length args) 2)
              (let* ((new-args (coerce-lower args type-args))
                     (proc2 (get op (map type-tag new-args))))
                (if proc2
                    (apply proc (contents new-args))
                    (error "no operation for args")))
              (error "no operation for args"))))))

