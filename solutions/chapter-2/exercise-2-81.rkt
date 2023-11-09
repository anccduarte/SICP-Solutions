
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.81.
;---
;Louis Reasoner has noticed that 'apply-generic' may try to coerce the arguments to each
;other's type even if they already have the same type. Therefore, he reasons, we need to
;put procedures in the coercion table to coerce arguments of each type to their own type.
;For example, in addition to the 'scheme-number->complex' coercion shown above, he would
;do:
;---
;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)
;---
;(a) With Louis's coercion procedures installed, what happens if 'apply-generic' is
;called with two arguments of type 'scheme-number' or two arguments of type 'complex' for
;an operation that is not found in the table for those types? For example, assume that
;we've defined a generic exponentiation operation:
;---
;(define (exp x y) (apply-generic 'exp x y))
;---
;and have put a procedure for exponentiation in the Scheme-number package but not in any
;other package:
;---
;(put 'exp
;     '(scheme-number scheme-number)
;     (lambda (x y) (tag (expt x y))))
;---
;What happens if we call 'exp' with two complex numbers as arguments?
;---
;(b) Is Louis correct that something had to be done about coercion with arguments of the
;same type, or does 'apply-generic' work correctly as is?
;---
;(c) Modify 'apply-generic' so that it doesn’t try coercion if the two arguments have the
;same type.
;------------------------------------------------------------------------------------------

;(a)
;---
;Without loss of generality, only the second question is answered. Assume that 'exp' is
;solely defined for '(scheme-number scheme-number). Moreover, assume that there exist
;coercion procedures defined for coercing scheme numbers into scheme numbers and complex
;numbers into complex numbers. Now, if we call 'exp' with two complex numbers as
;arguments, the set of keys {'exp, '(complex-number complex-number)} is exploited to look
;for a procedure 'exp' that knows how to deal with exponentiation involving two complex
;numbers. Such a procedure does not exist; hence (since the number of arguments is 2), we
;look for the procedures for coercing type 1 into type 2 and type 2 into type 1 (in both
;cases, 'complex->complex'). Since 'complex->complex' lays on the coercion table keyed by
;{'complex, 'complex}, the procedure is applied to the first argument (spuriously
;converting a complex number into a complex number) and the second argument is left
;exactly as is. 'apply-generic' is then applied to the new set of arguments. Yet, note
;that the arguments provided to 'apply-generic' are exactly the same as the ones provided
;in a first instance. It is now obvious that 'apply-generic' is condemned to be infinitly
;called with the same set of arguments, resulting in an infinite loop. 'apply-generic' is
;provided below for a clearer depiction of the problem at hand.
;---
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;(b)
;---
;Louis idea seems, in a first instance, to be pertinent. However, by closely inspecting
;the problem, we see that coercing a type to its own type is nonsensical. As previously
;observed, although we escape the error resulting from not finding the procedure we are
;looking for, we end up entering an infinite loop of redundant 'apply-generic' calls.
;'apply-generic' works fine exactly as is; however, it may be improved.

;(c)
;---
;Try coercing a type to its own type is, as previously stated, nonsensical, and therefore
;not incorporated in our system. Hence, there is no need to look for procedures in the
;coercion table that do this. In practice, such insight results in a slight change in
;'apply-generic': an additional clause is added to check whether its arguments are of the
;same type.
;---
(define (same-type? type1 type2)
  (eq? type1 type2))
;---
(define (apply-generic op . args)
  (let ((type-args (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (not (same-type? type1 type2))
                    (let ((arg1 (car args))
                          (arg2 (cadr args))
                          (t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic (t1->t2 arg1) arg2))
                            (t2->t1
                             (apply-generic arg1 (t2->t1 arg1)))
                            (else
                             (error "No method for these types"
                                     (list op type-tags)))))
                    (error "No method for these types"
                           (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

