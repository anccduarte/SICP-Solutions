
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.78.
;---
;The internal procedures in the 'scheme-number' package are essentially nothing more than
;calls to the primitive procedures '+', '-', etc. It was not possible to use the
;primitives of the language directly because our type-tag system requires that each data
;object have a type attached to it. In fact, however, all Lisp implementations do have a
;type system, which they use internally. Primitive predicates such as 'symbol?' and
;'number?' determine whether data objects have particular types. Modify the definitions
;of 'type-tag', 'contents', and 'attach-tag' from Section 2.4.2 so that our generic
;system takes advantage of Scheme's internal type system. That is to say, the system
;should work as before except that ordinary numbers should be represented simply as
;Scheme numbers rather than as pairs whose car is the symbol 'scheme-number'.
;------------------------------------------------------------------------------------------

;original implementation of the type-tag system
;---
(define (attach-tag-old type contents)
  (cons type contents))
;---
(define (type-tag-old datum)
  (if (pair? datum)
      (car datum)
      (error "bad tag:" datum)))
;---
(define (contents-old datum)
  (if (pair? datum)
      (cdr datum)
      (error "bad tag:" datum)))

;new procedures taking advantage of scheme's internal type system
;(note that the procedures would still be installed in the table filed under the symbol
;'scheme-number; however, the data object itself is not explicitly tagged: 'attach-tag'
;may remain untouched since it not utilized during installation...)
;---
(define (attach-tag type contents)
  (cons type contents))
;---
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "bad tag:" datum))))
;---
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "bad tag:" datum))))

;scheme numbers would simply be installed as follows
;(note that there is no need for a constructor, since the built-in type system is used to
;extract the tag and contents of a scheme number)
;---
(define (install-scheme-numbers)
  ;interface to rest of the system
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /))

;to confirm that the current implementation actually works, let's trace the operation of
;adding two random integers ('apply-generic' and 'add' are implemented below for a more
;clear visualization of the evolution of the process)
;---
(define (apply-generic op . args)
  (let ((type-args (map type-tag args)))
    (let ((proc (get op type-args)))
      (if proc
          (apply proc
                 (map contents args))
          (error "bad operation for data types:"
                 (list op type-args))))))
;---
(define (add x y)
  (apply-generic 'add x y))
;---
;1. (add 2 4)
;2. (apply-generic 'add 2 4)
;3. ((get 'add (map type-tag '(2 4))) (map contents '(2 4)))
;4. ((get 'add '(scheme-number 'scheme-number)) '(2 4))
;5. (+ 2 4)
;6. 6

