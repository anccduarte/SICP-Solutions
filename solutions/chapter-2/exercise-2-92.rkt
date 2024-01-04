
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.92.
;---
;By imposing an ordering on variables, extend the polynomial package so that addition and
;multiplication of polynomials works for polynomials in different variables. (This is not
;easy!)
;------------------------------------------------------------------------------------------

;---
;PRELIMINARY NOTES
;---
;1. The whole generic arithmetic package is implemented here (i.e., it includes the
;   implementation of integers, rationals, reals, complex numbers and polynomials). This
;   is relevant given the very convoluted nature of the exercise. Except for the
;   functionality proposed by the authors (see formulation above), the package is fully
;   operational.
;---
;2. Tables are implemented quite rudimentarily; a table simply consists of a list of
;   lists so that the elements of the inner lists are, from left to right (car to caddr),
;   the name of the operation, the data type(s) and the procedure itself (obviously
;   congruent to the pair {operation, data type(s)}). For a more detailed discussion on
;   tables, visit SICP's chapter 3.
;---
;3. The proposed functionality is not yet implemented. When Abelson and Sussman state,
;   about the exercise, that "This is not easy!", you may expect a tremendously difficult
;   endeavor.

;---
;TABLES [put & get] 
;---
(define (the-empty-table) '())
;---
(define (put table op type proc)
  (if (null? table)
      (list (list op type proc))
      (let ((entry (car table)))
        (if (and (eq? op (car entry))
                 (eq? type (cadr entry)))
            (cons (list op type proc)
                  (cdr table))
            (cons entry
                  (put (cdr table) op type proc))))))
;---
(define (get table op type)
  (if (null? table)
      #f
      (let ((entry (car table)))
        (if (and (eq? op (car entry))
                 (equal? type (cadr entry)))
            (caddr entry)
            (get (cdr table) op type)))))

;---
;TAGGING SYSTEM
;---
(define (attach-tag tag data)
  (cons tag data))
;---
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "TYPE-TAG: bad datum:"
             datum)))
;---
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "CONTENTS: bad datum:"
             datum)))

;---
;TOWER OF TYPES & POSITIONS
;---
(define tower '(integer rational real complex))
;---
(define (get-pos type tower-types)
  (define (pos-iter tower pos)
    (if (null? tower)
        (error "GET-POS: type not in tower of types:"
               (list type tower-types))
        (if (eq? (car tower) type)
            pos
            (pos-iter (cdr tower) (+ pos 1)))))
  (pos-iter tower-types 1))

;---
;'raise' [coercion]
;---
(define (repeated proc n)
  (if (= n 1)
      proc
      (lambda (x) (proc ((repeated proc (- n 1)) x)))))
;---
(define (raise arg n)
  ((repeated coerce n) arg))

;---
;'drop' [coercion]
;---
(define (bool? arg)
  (or (equal? arg #t) (equal? arg #f)))
;---
(define (drop arg)
  (if (or (bool? arg)
          (= (get-pos (type-tag arg) tower) 1))
      arg
      (let* ((proj-arg (project arg))
             (coerced-proj (coerce proj-arg)))
        (if (=? arg coerced-proj)
            (drop proj-arg)
            arg))))

;---
;'apply-generic'
;---
(define (get-diff arg1 arg2)
  (let ((pos1 (get-pos (type-tag arg1) tower))
        (pos2 (get-pos (type-tag arg2) tower)))
    (- pos1 pos2)))
;---
(define (apply-generic op . args)
  (let* ((type-args (map type-tag args))
         (proc (get ops-complete op type-args)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let* ((arg1 (car args))
                   (arg2 (cadr args))
                   (diff (get-diff arg1 arg2)))
              (cond ((< diff 0)
                     (apply-generic op
                                    (raise arg1 (abs diff))
                                    arg2))
                    ((> diff 0)
                     (apply-generic op
                                    arg1
                                    (raise arg2 diff)))
                    (else
                     (error "APPLY-GENERIC: no operation for args:"
                            (list op args)))))
            (error "APPLY-GENERIC: no operation for args:"
                   (list op args))))))

;---
;GENERIC OPERATORS
;---
(define (add x y) (apply-generic 'add x y))            ;generic
(define (sub x y) (apply-generic 'sub x y))            ;generic
(define (mul x y) (apply-generic 'mul x y))            ;generic
(define (div x y) (apply-generic 'div x y))            ;generic
(define (negate x) (apply-generic 'negate x))          ;generic
;---
(define (<? x y) (apply-generic '<? x y))              ;generic (- {poly})
(define (=? x y) (apply-generic '=? x y))              ;generic
(define (>? x y) (apply-generic '>? x y))              ;generic (- {poly})
(define (=zero? x) (apply-generic '=zero? x))          ;generic
;---
(define (absolute x) (apply-generic 'absolute x))      ;generic (- {poly})
(define (rem x y) (apply-generic 'rem x y))            ;integers
(define (rnd x) (apply-generic 'rnd x))                ;generic (- {poly})
(define (gcd x y) (apply-generic 'gcd x y))            ;integers
(define (sqroot x) (apply-generic 'sqroot x))          ;generic (- {comp, poly}) 
(define (square x) (apply-generic 'square x))          ;generic
;---
(define (arctan x y) (apply-generic 'arctan x y))      ;generic (- {comp, poly})
(define (sine x) (apply-generic 'sine x))              ;generic (- {comp, poly})
(define (cosine x) (apply-generic 'cosine x))          ;generic (- {comp, poly})
;---
(define (numer x) (apply-generic 'numer x))            ;generic (- {comp, poly})
(define (denom x) (apply-generic 'denom x))            ;generic (- {comp, poly})
;---
(define (to-float x) (apply-generic 'to-float x))      ;generic (- {comp, poly})
;---
(define (show x) (apply-generic 'show x))              ;generic
;---
(define (real-part z) (apply-generic 'real-part z))    ;complex
(define (imag-part z) (apply-generic 'imag-part z))    ;complex
(define (magnitude z) (apply-generic 'magnitude z))    ;complex
(define (angle z) (apply-generic 'angle z))            ;complex
;---
(define (coerce x) (apply-generic 'coerce x))          ;generic (- {poly})
(define (project x) (apply-generic 'project x))        ;generic (- {poly})
;---
(define (empty? t) (apply-generic 'empty? t))          ;polynomials
(define (first-term t) (apply-generic 'first-term t))  ;polynomials
(define (rest-terms t) (apply-generic 'rest-terms t))  ;polynomials

;---
;INTEGER NUMBERS
;---
(define (install-integer-package ops)
  ;---
  ;internal procedures
  ;---
  (define (tag i) (attach-tag 'integer i))
  ;---
  (define (make-integer x)
    (if (number? x)
        (if (integer? x)
            (inexact->exact x)
            (inexact->exact (round x)))
        (error "MAKE-INT: arg must be a number:" x)))
  ;---
  (define (gcd-int a b)
    (if (= b 0)
        a
        (gcd-int b (remainder a b))))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'add '(integer integer)
                     (lambda (x y) (tag (+ x y)))))
         (ops02 (put ops01 'sub '(integer integer)
                     (lambda (x y) (tag (- x y)))))
         (ops03 (put ops02 'mul '(integer integer)
                     (lambda (x y) (tag (* x y)))))
         (ops04 (put ops03 'div '(integer integer)
                     (lambda (x y) (make-real (/ x y)))))
         (ops05 (put ops04 'negate '(integer)
                     (lambda (x) (tag (- x)))))
         ;---
         (ops06 (put ops05 '<? '(integer integer)
                     (lambda (x y) (< x y))))
         (ops07 (put ops06 '=? '(integer integer)
                     (lambda (x y) (= x y))))
         (ops08 (put ops07 '>? '(integer integer)
                     (lambda (x y) (> x y))))
         (ops09 (put ops08 '=zero? '(integer)
                     (lambda (x) (= x 0))))
         ;---
         (ops10 (put ops09 'absolute '(integer)
                     (lambda (x) (tag (abs x)))))
         (ops11 (put ops10 'rem '(integer integer)
                     (lambda (x y) (tag (remainder x y)))))
         (ops12 (put ops11 'rnd '(integer)
                     (lambda (x) (tag (round x)))))
         (ops13 (put ops12 'gcd '(integer integer)
                     (lambda (x y) (tag (gcd-int x y)))))
         (ops14 (put ops13 'sqroot '(integer)
                     (lambda (x) (make-real (sqrt x)))))
         (ops15 (put ops14 'square '(integer)
                     (lambda (x) (tag (* x x)))))
         ;---
         (ops16 (put ops15 'arctan '(integer integer)
                     (lambda (x y) (make-real (atan x y)))))
         (ops17 (put ops16 'sine '(integer)
                     (lambda (x) (make-real (sin x)))))
         (ops18 (put ops17 'cosine '(integer)
                     (lambda (x) (make-real (cos x)))))
         ;---
         (ops19 (put ops18 'numer '(integer)
                     (lambda (x) (tag x))))
         (ops20 (put ops19 'denom '(integer)
                     (lambda (x) (tag 1))))
         ;---
         (ops21 (put ops20 'to-float '(integer)
                     (lambda (x) (make-real (/ x 1.0))))) ;[complex]
         ;---
         (ops22 (put ops21 'make 'integer
                     (lambda (x) (tag (make-integer x)))))
         ;---
         (ops23 (put ops22 'show '(integer)
                     (lambda (x) (display x))))
         ;---
         (ops24 (put ops23 'coerce '(integer) ;to-rational
                     (lambda (x) (make-rat (tag x) (tag 1))))))
    ;---
    (display "DONE [install-integer-package]") (newline)
    ops24))

;---
;RATIONAL NUMBERS
;---
(define (install-rational-package ops)
  ;---
  ;internal procedures
  ;---
  (define (tag r) (attach-tag 'rational r))
  ;---
  (define (make-rational n d)
    ;---
    (define (verify-args)
      (and (integer? (contents n))
           (integer? (contents d))
           (not (=zero? d))))
    ;---
    (if (verify-args)
        (if (<? d (make-int 0))
            (make-rational (negate n) (negate d))
            (let ((g (gcd (absolute n) d)))
              (cons (rnd (div n g)) (rnd (div d g)))))
        (error "MAKE-RAT: args not of type 'integer' or denom equals 0:"
               (list n d))))
  ;---
  (define (numer-rat r) (car r))
  (define (denom-rat r) (cdr r))
  ;---
  (define (add-rat r1 r2)
    (make-rational (add (mul (numer-rat r1) (denom-rat r2))
                        (mul (numer-rat r2) (denom-rat r1)))
                   (mul (denom-rat r1) (denom-rat r2))))
  ;---
  (define (sub-rat r1 r2)
    (add-rat r1
             (make-rational (negate (numer-rat r2)) (denom-rat r2))))
  ;---
  (define (mul-rat r1 r2)
    (make-rational (mul (numer-rat r1) (numer-rat r2))
                   (mul (denom-rat r1) (denom-rat r2))))
  ;---
  (define (div-rat r1 r2)
    (mul-rat r1
             (make-rational (denom-rat r2) (numer-rat r2))))
  ;---
  (define (negate r)
    (make-rational (negate (numer-rat r)) (denom-rat r)))
  ;---
  (define (is-less? r1 r2)
    (<? (div (numer-rat r1) (denom-rat r1))
        (div (numer-rat r2) (denom-rat r2))))
  ;---
  (define (is-equal? r1 r2)
    (=? (mul (numer-rat r1) (denom-rat r2))
        (mul (numer-rat r2) (denom-rat r1))))
  ;---
  (define (is-greater? r1 r2)
    (>? (div (numer-rat r1) (denom-rat r1))
             (div (numer-rat r2) (denom-rat r2))))
  ;---
  (define (is-zero? r)
    (let ((res (div (numer-rat r) (denom-rat r))))
      (<? (absolute res) (make-real 0.00001))))
  ;---
  (define (absol-rat r)
    (make-rat (absolute (numer-rat r)) (absolute (denom-rat r))))
  ;---
  (define (square-rat r)
    (make-rat (square (numer-rat r)) (square (denom-rat r))))
  ;---
  (define (atan-rat r1 r2)
    (arctan (div (numer-rat r1) (denom-rat r1))
            (div (numer-rat r2) (denom-rat r1))))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'add '(rational rational)
                     (lambda (r1 r2) (tag (add-rat r1 r2)))))
         (ops02 (put ops01 'sub '(rational rational)
                     (lambda (r1 r2) (tag (sub-rat r1 r2)))))
         (ops03 (put ops02 'mul '(rational rational)
                     (lambda (r1 r2) (tag (mul-rat r1 r2)))))
         (ops04 (put ops03 'div '(rational rational)
                     (lambda (r1 r2) (tag (div-rat r1 r2)))))
         (ops05 (put ops04 'negate '(rational)
                     (lambda (r) (tag (negate r)))))
         ;---
         (ops06 (put ops05 '<? '(rational rational)
                     (lambda (r1 r2) (is-less? r1 r2))))
         (ops07 (put ops06 '=? '(rational rational)
                     (lambda (r1 r2) (is-equal? r1 r2))))
         (ops08 (put ops07 '>? '(rational rational)
                     (lambda (r1 r2) (is-greater? r1 r2))))
         (ops09 (put ops08 '=zero? '(rational)
                     (lambda (r) (is-zero? r))))
         ;---
         (ops10 (put ops09 'absolute '(rational)
                     (lambda (r) (tag (absol-rat r)))))
         (ops11 (put ops10 'rnd '(rational)
                     (lambda (r) (rnd (div (numer-rat r) (denom-rat r))))))
         (ops12 (put ops11 'sqroot '(rational)
                     (lambda (r) (sqroot (div (numer-rat r) (denom-rat r))))))
         (ops13 (put ops12 'square '(rational)
                     (lambda (r) (tag (square-rat r)))))
         ;---
         (ops14 (put ops13 'arctan '(rational rational)
                     (lambda (r1 r2) (atan-rat r1 r2))))
         (ops15 (put ops14 'sine '(rational)
                     (lambda (r) (sine (div (numer-rat r) (denom-rat r))))))
         (ops16 (put ops15 'cosine '(rational)
                     (lambda (r) (cosine (div (numer-rat r) (denom-rat r))))))
         ;---
         (ops17 (put ops16 'numer '(rational)
                     (lambda (r) (numer-rat r))))
         (ops18 (put ops17 'denom '(rational)
                     (lambda (r) (denom-rat r))))
         ;---
         (ops19 (put ops18 'to-float '(rational)
                     (lambda (r) (div (numer-rat r) (denom-rat r))))) ;[complex]
         ;---
         (ops20 (put ops19 'make 'rational
                     (lambda (n d) (tag (make-rational n d)))))
         ;---
         (ops21 (put ops20 'show '(rational)
                     (lambda (x)
                       (show (numer-rat x)) (display "/") (show (denom-rat x)))))
         ;---
         (ops22 (put ops21 'coerce '(rational) ;to-real
                    (lambda (r) (div (numer-rat r) (denom-rat r)))))
         ;---
         (ops23 (put ops22 'project '(rational) ;to-integer
                    (lambda (r) (rnd (div (numer-rat r) (denom-rat r)))))))
    ;---
    (display "DONE [install-rational-package]") (newline)
    ops23))

;---
;REAL NUMBERS
;---
(define (install-real-package ops)
  ;---
  ;internal procedures
  ;---
  (define (tag r) (attach-tag 'real r))
  ;---
  (define (make-real-internal x)
    (if (number? x)
        x
        (error "MAKE-REAL: arg must a number:"
               x)))
  ;---
  (define (is-zero? x)
    (< (abs x) 0.00001))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'add '(real real)
                    (lambda (x y) (tag (+ x y)))))
         (ops02 (put ops01 'sub '(real real)
                     (lambda (x y) (tag (- x y)))))
         (ops03 (put ops02 'mul '(real real)
                     (lambda (x y) (tag (* x y)))))
         (ops04 (put ops03 'div '(real real)
                     (lambda (x y) (tag (/ x y)))))
         (ops05 (put ops04 'negate '(real)
                     (lambda (x) (tag (- x)))))
         ;---
         (ops06 (put ops05 '<? '(real real)
                     (lambda (x y) (< x y))))
         (ops07 (put ops06 '>? '(real real)
                     (lambda (x y) (> x y))))
         (ops08 (put ops07 '=zero? '(real)
                     (lambda (x) (is-zero? x))))
         (ops09 (put ops08 '=? '(real real)
                     (lambda (x y) (is-zero? (- x y)))))
         ;---
         (ops10 (put ops09 'absolute '(real)
                     (lambda (x) (tag (abs x)))))
         (ops11 (put ops10 'rnd '(real)
                     (lambda (x) (make-int x))))
         (ops12 (put ops11 'sqroot '(real)
                     (lambda (x) (tag (sqrt x)))))
         (ops13 (put ops12 'square '(real)
                     (lambda (x) (tag (* x x)))))
         ;---
         (ops14 (put ops13 'arctan '(real real)
                     (lambda (x y) (tag (atan x y)))))
         (ops15 (put ops14 'sine '(real)
                     (lambda (x) (tag (sin x)))))
         (ops16 (put ops15 'cosine '(real)
                     (lambda (x) (tag (cos x)))))
         ;---
         (ops17 (put ops16 'numer '(real)
                     (lambda (x) (make-int (numerator x)))))
         (ops18 (put ops17 'denom '(real)
                     (lambda (x) (make-int (denominator x)))))
         ;---
         (ops19 (put ops18 'to-float '(real)
                     (lambda (x) (tag x)))) ;identity [complex]
         ;---
         (ops20 (put ops19 'make 'real
                     (lambda (x) (tag (make-real-internal x)))))
         ;---
         (ops21 (put ops20 'show '(real)
                     (lambda (x) (display x))))
         ;---
         (ops22 (put ops21 'coerce '(real) ;to-complex
                     (lambda (x) (make-rect (make-real x) (make-real 0)))))
         ;---
         (ops23 (put ops22 'project '(real) ;to-rational
                     (lambda (x) (make-rat (make-int (numerator x))
                                           (make-int (denominator x)))))))
    ;---
    (display "DONE [install-real-package]") (newline)
    ops23))

;---
;COMPLEX NUMBERS [subpackages]
;---
(define (install-rectangular-package ops)
  ;---
  ;internal procedures
  ;---
  (define (tag z) (attach-tag 'rectangular z))
  ;---
  (define (valid-arg? x)
    (or (eq? (type-tag x) 'integer)
        (eq? (type-tag x) 'rational)
        (eq? (type-tag x) 'real)))
  ;---
  (define (make-rect r i)
    (if (and (valid-arg? r) (valid-arg? i))
        (cons r i)
        (error "MAKE-RECT: arg(s) not of valid type:"
               (list r i))))
  ;---
  (define (make-polar r i)
    (if (and (valid-arg? r) (valid-arg? i))
        (cons (sqroot (add (square r) (square i))) (arctan i r))
        (error "MAKE-POLAR: arg(s) not of valid type:"
               (list r i))))
  ;---
  (define (magnitude z)
    (sqroot (add (square (car z)) (square (cdr z)))))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'real-part '(rectangular)
                     (lambda (z) (car z))))
         (ops02 (put ops01 'imag-part '(rectangular)
                     (lambda (z) (cdr z))))
         (ops03 (put ops02 'magnitude '(rectangular)
                     (lambda (z) (magnitude z))))
         (ops04 (put ops03 'angle '(rectangular)
                     (lambda (z) (arctan (cdr z) (car z)))))
         ;---
         (ops05 (put ops04 'make-rect 'rectangular
                     (lambda (r i) (tag (make-rect r i)))))
         (ops06 (put ops05 'make-polar 'rectangular
                     (lambda (r i) (tag (make-polar r i))))))
    ;---
    (display "DONE [install-rectangular-package]") (newline)
    ops06))
;---
(define (install-polar-package ops)
  ;---
  ;internal procedures
  ;---
  (define (tag z) (attach-tag 'polar z))
  ;---
  (define (valid-arg? x)
    (or (eq? (type-tag x) 'integer)
        (eq? (type-tag x) 'rational)
        (eq? (type-tag x) 'real)))
  ;---
  (define (make-rect m a)
    (if (and (valid-arg? m) (valid-arg? a))
        (cons (mul m (cosine a)) (mul m (sine a)))
        (error "MAKE-RECT: arg(s) not of valid type:"
               (list m a))))
  (define (make-polar m a)
    (if (and (valid-arg? m) (valid-arg? a))
        (cons m a)
        (error "MAKE-POLAR: arg(s) not of valid type:"
               (list m a))))
  ;---
  (define (real-part z) (mul (car z) (cosine (cdr z))))
  (define (imag-part z) (mul (car z) (sine (cdr z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'real-part '(polar)
                     (lambda (z) (mul (car z) (cosine (cdr z))))))
         (ops02 (put ops01 'imag-part '(polar)
                     (lambda (z) (mul (car z) (sine (cdr z))))))
         (ops03 (put ops02 'magnitude '(polar)
                     (lambda (z) (car z))))
         (ops04 (put ops03 'angle '(polar)
                     (lambda (z) (cdr z))))
         ;---
         (ops05 (put ops04 'make-rect 'polar
                     (lambda (m a) (tag (make-rect m a)))))
         (ops06 (put ops05 'make-polar 'polar
                     (lambda (m a) (tag (make-polar m a))))))
    ;---
    (display "DONE [install-polar-package]") (newline)
    ops06))

;---
;COMPLEX NUMBERS [main]
;---
(define (install-complex-package ops)
  ;---
  ;imports from 'rectangular' and 'polar'
  ;---
  (define make-rect
    (get ops-int-rat-real-rec-pol 'make-rect 'rectangular))
  (define make-polar
    (get ops-int-rat-real-rec-pol 'make-polar 'polar))
  ;---
  ;internal procedures
  ;---
  (define (tag z) (attach-tag 'complex z))
  ;---
  (define (add-comp z1 z2)
    (make-rect (add (real-part z1) (real-part z2))
               (add (imag-part z1) (imag-part z2))))
  ;---
  (define (sub-comp z1 z2)
    (make-rect (sub (real-part z1) (real-part z2))
               (sub (imag-part z1) (imag-part z2))))
  ;---
  (define (mul-comp z1 z2)
    (make-polar (mul (magnitude z1) (magnitude z2))
                (add (angle z1) (angle z2))))
  ;---
  (define (div-comp z1 z2)
    (make-polar (div (magnitude z1) (magnitude z2))
                (sub (angle z1) (angle z2))))
  ;---
  (define (neg-comp z)
    (make-rect (negate (real-part z))
               (negate (imag-part z))))
  ;---
  (define (is-zero? z)
    (and (=zero? (real-part z)) (=zero? (imag-part z))))
  ;---
  (define (abs-complex z)
    (make-rect (absolute (real-part z)) (absolute (imag-part z))))
  ;---
  (define (rnd-complex z)
    (make-rect (rnd (real-part z)) (rnd (imag-part z))))
  ;---
  (define (sq-complex z)
    (make-rect (sub (square (real-part z)) (square (imag-part z)))
               (mul (make-int 2) (real-part z) (imag-part z))))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'real-part '(complex)
                     (lambda (z) (real-part z))))
         (ops02 (put ops01 'imag-part '(complex)
                     (lambda (z) (imag-part z))))
         (ops03 (put ops02 'magnitude '(complex)
                     (lambda (z) (magnitude z))))
         (ops04 (put ops03 'angle '(complex)
                     (lambda (z) (angle z))))
         ;---
         (ops05 (put ops04 'add '(complex complex)
                     (lambda (z1 z2) (tag (add-comp z1 z2)))))
         (ops06 (put ops05 'sub '(complex complex)
                     (lambda (z1 z2) (tag (sub-comp z1 z2)))))
         (ops07 (put ops06 'mul '(complex complex)
                     (lambda (z1 z2) (tag (mul-comp z1 z2)))))
         (ops08 (put ops07 'div '(complex complex)
                     (lambda (z1 z2) (tag (div-comp z1 z2)))))
         (ops09 (put ops08 'negate '(complex)
                     (lambda (z) (tag (neg-comp z)))))
         ;---
         (ops10 (put ops09 '<? '(complex complex)
                     (lambda (z1 z2) (<? (magnitude z1) (magnitude z2)))))
         (ops11 (put ops10 '=? '(complex complex)
                     (lambda (z1 z2) (=? (magnitude z1) (magnitude z2)))))
         (ops12 (put ops11 '>? '(complex complex)
                     (lambda (z1 z2) (>? (magnitude z1) (magnitude z2)))))
         (ops13 (put ops12 '=zero? '(complex)
                     (lambda (z) (is-zero? z))))
         ;---
         (ops14 (put ops13 'absolute '(complex)
                     (lambda (z) (tag (abs-complex z)))))
         (ops15 (put ops14 'rnd '(complex)
                     (lambda (z) (tag (rnd-complex z)))))
         (ops16 (put ops15 'square '(complex)
                     (lambda (z) (tag (sq-complex z)))))
         ;---
         (ops17 (put ops16 'make-rect 'complex
                     (lambda (r i) (tag (make-rect r i)))))
         (ops18 (put ops17 'make-polar 'complex
                     (lambda (m a) (tag (make-polar m a)))))
         (ops19 (put ops18 'show '(complex)
                     (lambda (z)
                       (show (real-part z)) (display " + ")
                       (show (imag-part z)) (display "i"))))
         ;---
         (ops20 (put ops19 'project '(complex)
                     (lambda (z) (to-float (real-part z))))))
    ;---
    (display "DONE [install-complex-package]") (newline)
    ops20))

;---
;POLYNOMIALS [subpackages]
;---
(define (install-sparse-package ops)
  ;---
  ;internal procedures
  ;---
  (define (tag t) (attach-tag 'sparse-term-list t))
  ;---
  (define (first term-list) (car term-list))
  (define (rest term-list) (cdr term-list))
  ;---
  (define (is-empty? term-list)
    (if (null? term-list)
        #t
        (let ((curr (first term-list)))
          (if (not (=zero? (cadr curr)))
              #f
              (is-empty? (rest term-list))))))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'make 'sparse-term-list
                     (lambda (t) (tag t))))
         (ops02 (put ops01 'make 'sparse-term
                     (lambda (order coeff) (list order coeff))))
         ;---
         (ops03 (put ops02 'empty? '(sparse-term-list)
                     (lambda (t) (is-empty? t))))
         ;---
         (ops04 (put ops03 'first-term '(sparse-term-list)
                     (lambda (t) (first t))))
         (ops05 (put ops04 'rest-terms '(sparse-term-list)
                     (lambda (t) (tag (rest t))))))
    ;---
    (display "DONE [install-sparse-package]") (newline)
    ops05))
;---
(define (install-dense-package ops)
  ;---
  ;internal procedures
  ;---
  (define (tag t) (attach-tag 'dense-term-list t))
  ;---
  (define (first term-list)
    (list (make-int (- (length term-list) 1))
          (car term-list)))
  ;---
  (define (rest term-list) (cdr term-list))
  ;---
  ;---
  (define (is-empty? term-list)
    (if (null? term-list)
        #t
        (let ((curr (first term-list)))
          (if (not (=zero? (cadr curr)))
              #f
              (is-empty? (rest term-list))))))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'make 'dense-term-list
                     (lambda (t) (tag t))))
         (ops02 (put ops01 'make 'dense-term
                     (lambda (order coeff) coeff)))
         ;---
         (ops03 (put ops02 'empty? '(dense-term-list)
                     (lambda (t) (is-empty? t))))
         ;---
         (ops04 (put ops03 'first-term '(dense-term-list)
                     (lambda (t) (first t))))
         (ops05 (put ops04 'rest-terms '(dense-term-list)
                     (lambda (t) (tag (rest t))))))
    ;---
    (display "DONE [install-dense-package]") (newline)
    ops05))

;---
;POLYNOMIAL [main]
;---
(define (install-polynomial-package ops)
  ;---
  ;imports from 'sparse' and 'dense'
  ;---
  (define make-sparse-term
    (get ops-int-rat-real-comp-sprs-dns 'make 'sparse-term))
  (define make-dense-term
    (get ops-int-rat-real-comp-sprs-dns 'make 'dense-term))
  ;---
  (define make-sparse-term-list
    (get ops-int-rat-real-comp-sprs-dns 'make 'sparse-term-list))
  (define make-dense-term-list
    (get ops-int-rat-real-comp-sprs-dns 'make 'dense-term-list))
  ;---
  ;internal procedures
  ;---
  (define (tag p) (attach-tag 'polynomial p))
  ;---
  (define (make-sparse-poly var terms)
    (list var (make-sparse-term-list terms)))
  (define (make-dense-poly var terms)
    (list var (make-dense-term-list terms)))
  ;---
  (define (variable poly) (car poly))
  (define (term-list poly) (cadr poly))
  ;---
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;---
  (define (negate-terms term-list)
    (define (term-iter terms)
      (if (empty? terms)
          '()
          (let ((first (first-term terms)))
            (cons (make-sparse-term (order first)
                                    (negate (coeff first)))
                  (term-iter (rest-terms terms))))))
    (term-iter term-list))
  ;---
  (define (negate-poly poly)
    (make-sparse-poly (variable poly)
                      (negate-terms (term-list poly))))
  ;---
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  ;---
  (define (add-terms T1 T2)
    (cond ((empty? T1) (contents T2))
          ((empty? T2) (contents T1))
          (else
           (let ((t1 (first-term T1))
                 (t2 (first-term T2)))
             (cond ((>? (order t1) (order t2))
                    (adjoin-term t1
                                 (add-terms (rest-terms T1) T2)))
                   ((>? (order t2) (order t1))
                    (adjoin-term t2
                                 (add-terms T1 (rest-terms T2))))
                   (else
                    (adjoin-term (make-sparse-term (order t1)
                                                   (add (coeff t1) (coeff t2)))
                                 (add-terms (rest-terms T1) (rest-terms T2)))))))))
  ;---
  (define (add-poly p1 p2)
    (if (eq? (variable p1) (variable p2))
        (make-sparse-poly (variable p1)
                          (add-terms (term-list p1)
                                     (term-list p2)))
        (error "ADD-POLY: polynomials not in same variable:"
               (list p1 p2))))
  ;---
  (define (sub-poly p1 p2)
    (if (eq? (variable p1) (variable p2))
        (add-poly p1 (negate-poly p2))
        (error "SUB-POLY: polynomials not in same variable:"
               (list p1 p2))))
  ;---
  (define (mul-term-by-terms t T)
    (if (empty? T)
        '()
        (let ((first (first-term T)))
          (adjoin-term (make-sparse-term (add (order t) (order first))
                                         (mul (coeff t) (coeff first)))
                       (mul-term-by-terms t (rest-terms T))))))
  ;---
  (define (mul-terms T1 T2)
    (define (mul-iter t1 t2)
      (if (empty? t1)
          t1
          (make-sparse-term-list
           (add-terms (make-sparse-term-list
                       (mul-term-by-terms (first-term t1) t2))
                      (mul-iter (rest-terms t1) t2)))))
    (contents (mul-iter T1 T2)))
  ;---
  (define (mul-poly p1 p2)
    (if (eq? (variable p1) (variable p2))
        (make-sparse-poly (variable p1)
                          (mul-terms (term-list p1)
                                     (term-list p2)))
        (error "MUL-POLY: polynomials not in same variable:"
               (list p1 p2))))
  ;---
  (define (show-poly p)
    ;---
    (define (display-term var term)
      (cond ((=zero? (order term))
             (show (coeff term)))
            (else
             (show (coeff term)) (display var)
             (display "^") (show (order term)))))
    ;---
    (define (show-iter var terms)
      (let ((first (first-term terms))
            (rest (rest-terms terms)))
        (display-term var first)
        (cond ((empty? rest)
               (display ""))
              (else
               (display " + ")
               (show-iter var (rest-terms terms))))))
    ;---
    (show-iter (variable p) (term-list p)))
  ;---
  ;interface to rest of the system
  ;---
  (let* ((ops01 (put ops 'negate '(polynomial)
                     (lambda (p) (tag (negate-poly p)))))
         (ops02 (put ops01 'add '(polynomial polynomial)
                     (lambda (p1 p2) (tag (add-poly p1 p2)))))
         (ops03 (put ops02 'sub '(polynomial polynomial)
                     (lambda (p1 p2) (tag (sub-poly p1 p2)))))
         (ops04 (put ops03 'mul '(polynomial polynomial)
                     (lambda (p1 p2) (tag (mul-poly p1 p2)))))
         ;---
         (ops05 (put ops04 'make-sparse 'polynomial
                     (lambda (v t) (tag (make-sparse-poly v t)))))
         (ops06 (put ops05 'make-dense 'polynomial
                     (lambda (v t) (tag (make-dense-poly v t)))))
         (ops07 (put ops06 'show '(polynomial)
                     (lambda (p) (show-poly p)))))
    (display "DONE [install-polynomial-package]") (newline)
    ops07))

;---
;INSTALL OPERATIONS
;---
(define ops
  (the-empty-table))
;---
(define ops-int
  (install-integer-package ops))
;---
(define ops-int-rat
  (install-rational-package ops-int))
;---
(define ops-int-rat-real
  (install-real-package ops-int-rat))
;---
(define ops-int-rat-real-rec
  (install-rectangular-package ops-int-rat-real))
;---
(define ops-int-rat-real-rec-pol
  (install-polar-package ops-int-rat-real-rec))
;---
(define ops-int-rat-real-comp
  (install-complex-package ops-int-rat-real-rec-pol))
;---
(define ops-int-rat-real-comp-sprs
  (install-sparse-package ops-int-rat-real-comp))
;---
(define ops-int-rat-real-comp-sprs-dns
  (install-dense-package ops-int-rat-real-comp-sprs))
;---
(define ops-int-rat-real-comp-poly
  (install-polynomial-package ops-int-rat-real-comp-sprs-dns))
;---
(define ops-complete ops-int-rat-real-comp-poly)
(display "---") (newline)

;---
;EXTRACT CONSTRUCTORS
;---
(define make-int
  (get ops-complete 'make 'integer))
;---
(define make-rat
  (get ops-complete 'make 'rational))
;---
(define make-real
  (get ops-complete 'make 'real))
;---
(define make-rect
  (get ops-complete 'make-rect 'complex))
;---
(define make-polar
  (get ops-complete 'make-polar 'complex))
;---
(define make-sparse-poly
  (get ops-complete 'make-sparse 'polynomial))
;---
(define make-dense-poly
  (get ops-complete 'make-dense 'polynomial))

;---
;TESTS
;---
(define r1 (make-rat (make-int 6) (make-int 2)))
(define r2 (make-rat (make-int 4) (make-int 3)))
(show (add r1 r2)) (newline)
(display "---") (newline)
;---
(define z1 (make-rect (make-int 2) (make-int 7)))
(define z2 (make-rect (make-int 5) (make-int 2)))
(let ((res (add z1 z2)))
  (show res) (newline)
  (show (magnitude res)) (newline))
(display "---") (newline)
;---
(let ((res (add r2 z1)))
  (show res) (newline)
  (show (project res)) (newline)
  (show (coerce (project res))) (newline))
(display "---") (newline)
;---
(let ((p1 (make-dense-poly
           'x
           (list (make-int 1) (make-int 2) (make-int 3) (make-int 4))))
      (p2 (make-sparse-poly
           'x
           (list (list (make-int 5) (make-int 2))
                 (list (make-int 3) (make-int 1))
                 (list (make-int 2) (make-int 7))
                 (list (make-int 0) (make-int 1))))))
  (show p1) (newline)
  (show p2) (newline)
  (show (add p1 p2)) (newline)
  (show (sub p1 p2)) (newline)
  (show (mul p1 p2)) (newline))

