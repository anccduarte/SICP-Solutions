
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.82.
;---
;Show how to generalize 'apply-generic' to handle coercion in the general case of
;multiple arguments. One strategy is to attempt to coerce all the arguments to the type
;of the first argument, then to the type of the second argument, and so on. Give an
;example of a situation where this strategy (and likewise the two-argument version given
;above) is not sufficiently general. (Hint: Consider the case where there are some
;suitable mixed-type operations present in the table that will not be tried.)
;------------------------------------------------------------------------------------------

;Note: In the answer it is assumed that multi-step coercion routines (i.e., involving the
;application of more than one coercion procedure) are prohibited.
;---
;Assume the following tower of types (the one presented by the authors in the book)
;---
; complex numbers
;       ∧
;  real numbers
;       ∧
;   rationals
;       ∧
;   integers
;---
;Now, assume that there are coercion procedures that specify how to coerce each type to
;the type that immediately follows it in the tower (type->supertype). Furthermore, assume
;that there is a generic operation 'op' of three numeric arguments. Considering the
;proposed coercion strategy, if, for example, 'op' is called with arguments of types
;'integer', 'real' and 'complex', 'apply-generic' will fail to apply 'op'. Although,
;quite evidently, there is a way to coerce the types of the first and second arguments
;('integer' and 'real', respectively) to the type of the third argument ('complex'), that
;is, 'integer->rational->real->complex' and 'real->complex', such coercion mechanism will
;never be tried. In this simple case (i.e., given the tower of types proposed above), for
;any operation to properly work, it must be invoked with arguments whose types lie in an
;"interval" of the tower of types with length not greater than two. That is to say that
;any operation invoked with arguments whose types may be subsetted in {integer, real},
;{integer, complex} or {rational, complex} will fail to be properly executed.

;Nonetheless, 'apply-generic' may be generalized in order to "properly" function when the
;operation to be applied takes more than two arguments. Remember that it will not be of
;any service whenever the previously reported circumstance occurs. The following
;procedure simply replicates SICP's 'apply-generic' for more than two arguments. The
;incoveniences shown by the latter persist in the immediate implementation. The following
;implementation is heavily inspired by JoT's Jottings solution. For extra details, visit
;http://jots-jottings.blogspot.com/2012/02/sicp-exercise-282-multi-argument.html.

;helper procedures
;---
(define (attach-tag type data)
  (cons type data))
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
;---
(define (belongs? elem lst)
  (cond ((null? lst)
         #f)
        ((eq? (car lst) elem)
         #t)
        (else
         (belongs? elem (cdr lst)))))
;---
(define (add-elem elem lst)
  (append lst (list elem)))

;'apply-generic'
;---
(define (apply-generic op . args)
  ;---
  (define (get-unique-types types)
    (define (types-iter types result)
      (if (null? types)
          result
          (if (belongs? (car types) result)
              (types-iter (cdr types)
                          result)
              (types-iter (cdr types)
                          (add-elem (car types) result)))))
    (types-iter types '()))
  ;---
  (define (coerce-to target-type args result)
    (if (null? args)
        result
        (let ((original-type (type-tag (car args))))
          (if (eq? target-type original-type)
              (coerce-to target-type
                         (cdr args)
                         (add-elem (car args) result))
              (let ((proc (get-coercion original-type target-type)))
                (if proc
                    (coerce-to target-type
                               (cdr args)
                               (add-elem (proc (car args)) result))
                    #f))))))
  ;---
  (define (apply-generic-iter types)
    (if (null? types)
        (error "no operation for these types")
        (let ((coerced-args (coerce-to (car types) args '())))
          (if coerced-args
              (let ((proc (get op (map type-tag coerced-args))))
                (if proc
                    (apply proc coerced-args)
                    (apply-generic-iter (cdr types))))
              (apply-generic-iter (cdr types))))))
  ;---
  (define (main)
    (let ((type-args (map type-tag args)))
      (let ((proc (get op type-args)))
        (if proc
            (apply proc (map contents args))
            (apply-generic-iter (get-unique-types type-args))))))
  ;---
  (main))

