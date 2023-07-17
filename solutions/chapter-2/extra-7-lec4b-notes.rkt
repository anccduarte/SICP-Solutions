
#lang sicp

;------------------------------------------------------------------------------------------
; GENERIC OPERATIONS
;------------------------------------------------------------------------------------------

; constructing typed data
; ---
(define (attach-type type contents)
  (cons type contents))
; ---
(define (type datum)
  (car datum))
; ---
(define (contents datum)
  (cdr datum))

; implementing tables
; (very crude and inefficient implementation)
; ---
(define (empty-table) '())
; ---
(define (table-key1 entry)
  (car entry))
; ---
(define (table-key2 entry)
  (cadr entry))
; ---
(define (table-value entry)
  (caddr entry))
; ---
(define (put table key1 key2 value)
  (cond ((null? table)
         (cons (list key1 key2 value) '()))
        ((and (eq? key1 (table-key1 (car table)))
              (eq? key2 (table-key2 (car table))))
         (cons (list key1 key2 value) (cdr table)))
        (else
         (cons (car table)
               (put (cdr table) key1 key2 value)))))
; ---
(define (get table key1 key2)
  (cond ((null? table)
         '())
        ((and (eq? key1 (table-key1 (car table)))
              (eq? key2 (table-key2 (car table))))
         (table-value (car table)))
        (else
         (get (cdr table) key1 key2))))

; 'operate' performs an operation 'op' on an object 'obj'
; ---
(define (operate table op obj)
  (let ((proc (get table (type obj) op)))
    (if (not (null? proc))
        (proc (contents obj))
        (error "undefined type or operator..."))))

; 'operate-2' performs an operation 'op' on two objects 'x' and 'y'
; ---
(define (operate-2 table op x y)
  (if (eq? (type x) (type y))
      (let ((proc (get table (type x) op)))
        (if (not (null? proc))
            (proc (contents x) (contents y))
            (error "undefined type or operator...")))
      (error "objects not of the same type")))

;------------------------------------------------------------------------------------------
; COMPLEX NUMBER REPRESENTATIONS
;------------------------------------------------------------------------------------------

; representing complex numbers as pairs real-part, imaginary-part
; (george's implementation -> better when dealing with additions and subtractions)
; ---
; (define (make-rectangular x y) ;obsolete -> added 'attach-type' below 
;   (cons x y))
; ---
(define (real-part-rectangular z)
  (car z))
; ---
(define (imag-part-rectangular z)
  (cdr z))
; ---
(define (magnitude-rectangular z)
  (define (square x) (* x x))
  (sqrt (+ (square (car z))
           (square (cdr z)))))
; ---
(define (angle-rectangular z)
  (atan (cdr z) (car z)))

; representing complex numbers as pairs magnitude, angle
; (martha's implementation -> better when dealing with multiplications and divisions)
; ---
; (define (make-polar r a) ;obsolete -> added 'attach-type' below
;   (cons r a))
; ---
(define (magnitude-polar z)
  (car z))
; ---
(define (angle-polar z)
  (cdr z))
; ---
(define (real-part-polar z)
  (* (car z) (cos (cdr z))))
; ---
(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

; we would like a system that can accomodate both representations; a system that allows
; for a representation to not interfere with the other
; ---
;       +c    -c    *c    /c    <----    USE (operations on complex numbers)
; -------------------------------------------------------------------------------
;              real-part    imag-part    magnitude    angle
; ------------------------------------++-----------------------------------------
;                      rectangular    ||    polar    <----    REPRESENTATION
;                                     ||

; to do that, we add a type to the complex number upon building it!
; ---
(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))
; ---
(define (make-polar r a)
  (attach-type 'polar (cons r a)))

; predicates for determining whether a complex number was built under george's (rect) or
; martha's (polar) representation
; ---
; (define (rectangular? z) ;obsolete -> dealt with by 'operate' below
;   (eq? (type z) 'rectangular))
; ---
; (define (polar? z) ;obsolete -> dealt with by 'operate' below
;   (eq? (type z) 'polar))

; now, we can have generic selectors 'real-part', 'imag-part', 'magnitude' and 'angle'
; that deal with both representations (a manager for dealing with both representations
; is implemented below). the strategy used for the implementation of generic selectors is
; called DISPATCH ON TYPE ("checks the type of the data and then dispatches it to the
; right selector representation")
; ---
; (define (real-part z) ;obsolete -> in terms of 'operate' below
;   (cond ((rectangular? z)
;          (real-part-rectangular (contents z)))
;         ((polar? z)
;          (real-part-polar (contents z)))
;         (else
;          (error "bad type:" (type z)))))
; ---
; (define (imag-part z) ;obsolete -> in terms of 'operate' below
;   (cond ((rectangular? z)
;          (imag-part-rectangular (contents z)))
;         ((polar? z)
;          (imag-part-polar (contents z)))
;         (else
;          (error "bad type:" (type z)))))
; ---
; (define (magnitude z) ;obsolete -> in terms of 'operate' below
;   (cond ((rectangular? z)
;          (magnitude-rectangular (contents z)))
;         ((polar? z)
;          (magnitude-polar (contents z)))
;         (else
;          (error "bad type:" (type z)))))
; ---
; (define (angle z) ;obsolete -> in terms of 'operate' below
;   (cond ((rectangular? z)
;          (angle-rectangular (contents z)))
;         ((polar? z)
;          (angle-polar (contents z)))
;         (else
;          (error "bad type:" (type z)))))

; the operations on complex numbers (USE) would then be implemeneted in terms of these
; generic selectors. see how the representation is completely isolated from the use
; ('real-part' and 'imag-part' call the adequate procedure based on the tag of its
; arguments - 'rectangular' or 'polar')
; ---
(define (+c z1 z2)
  (make-rectangular
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))
; ---
(define (-c z1 z2)
  (make-rectangular
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))
; ---
(define (*c z1 z2)
  (make-polar
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))
; ---
(define (/c z1 z2)
  (make-polar
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

; criticisms on this strategy (manager) and alternate solution
; ---
; "Imagine Harry comes in with some new kind of complex number. Now, the manager has to
; go in and change all those procedures [the generic selectors]. So, the inflexibility in
; the system, the place where work has to happen to accomodate change, is in the manager.
; That's pretty annoying. It's even more annoying when you realize the manager is not
; doing anything. The manager is just being a paper pusher. Let's look again at these
; programs. What are they doing? What does 'real-part' do? 'real-part' says 'Oh, is it
; the kind of complex number that George can handle? If so, send it off to George. Is it
; the kind of complex number that Martha can handle? If so, send it off to Martha.' So,
; it is really annoying that the bottleneck in this system, the thing that's preventing
; flexibility and change, is completely in the bureaucracy. It's not in anybody who's
; doing any of the work. So, what's really going on? Abstractly, in this system, there's
; a table. There are types [rectangular and polar] and then there are operators
; [real-part, imag-part, magnitude and angle], and sitting in this table are the right
; procedures [at the right row and column]. In some sense, all the manager is doing is
; acting as this table. Well, how do we fix our system? What you do is you get rid of the
; manager. We just take the manager and replace him by a computer. We're going to
; automate him out of existence. Namely, instead of having the manager who basically
; consults this table, we'll have our system use the table directly. Given this table,
; what do George and Martha have to do? Well, when they build their system, they each
; have the responsibility to set up their appropriate column in the table. So, what
; George does, for example, when he defines his procedures, is go off and put into the
; table the procedures for the type 'rectangular' [the representation chosen by George].
; Well, what happened to the manager? The manager has been automated out of existence and
; is replaced by a procedure called 'operate'. And this is the key procedure in the whole
; [complex number] system." [SEE OPERATIONS ON TABLES AND THE DEFINITION OF 'OPERATE'
; UNDER THE SECTION 'GENERIC OPERATIONS']

; creating table and installing rectangular and polar operations
; ---
(define complex-table-init (empty-table))
; ---
(define complex-table-1
  (put complex-table-init 'rectangular 'real-part real-part-rectangular))
; ---
(define complex-table-2
  (put complex-table-1 'rectangular 'imag-part imag-part-rectangular))
; ---
(define complex-table-3
  (put complex-table-2 'rectangular 'magnitude magnitude-rectangular))
; ---
(define complex-table-4
  (put complex-table-3 'rectangular 'angle angle-rectangular))
; ---
(define complex-table-5
  (put complex-table-4 'polar 'real-part real-part-polar))
; ---
(define complex-table-6
  (put complex-table-5 'polar 'imag-part imag-part-polar))
; ---
(define complex-table-7
  (put complex-table-6 'polar 'magnitude magnitude-polar))
; ---
(define complex-table
  (put complex-table-7 'polar 'angle angle-polar))

; now, we may define our selector operations in terms of 'operate'! this way of
; implementing generic operators is called DATA-DIRECTED PROGRAMMING ("the idea is that,
; in some sense, the data objects themselves, those little complex numbers that are
; floating around the system, are carrying with them the information about how you should
; operate on them")
; ---
(define (real-part obj)
  (operate complex-table 'real-part obj))
; ---
(define (imag-part obj)
  (operate complex-table 'imag-part obj))
; ---
(define (magnitude obj)
  (operate complex-table 'magnitude obj))
; ---
(define (angle obj)
  (operate complex-table 'angle obj))

; example demonstrating what actually happens when a generic selector is called. assume
; that 'z' is a complex number created by (make-polar 1 2). calling 'real-part' on 'z'
; yields the following
; ---
; (define z (make-polar 1 2))
; (real-part z)
; (operate complex-table 'real-part z)
; ((get complex-table (type z) 'real-part) (contents z))
; ((get complex-table 'polar 'real-part) (contents z))
; (real-part-polar (contents z))
; (real-part-polar (1 . 2))

; now, imagine that we want our complex number package to be part of a more complex
; package that performs operations on arbitrary types of numbers. it would look something
; like the following
; ---
;                   ADD          SUB          MUL          DIV
; ----------------------||------------------------------||------------------------
;        RATIONAL       ||       COMPLEX NUMBERS        ||        REGULAR
;        NUMBERS        ||      +c   -c   *c   /c       ||        NUMBERS
;                       ||--------------||--------------||        
;  +rat -rat *rat /rat  ||    RECT      ||    POLAR     ||      +   -   *   /
;                       ||              ||              ||

; how would we construct generic operators for 'ADD', 'SUB', 'MUL' and 'DIV', then? the
; exact same way we constructed generic operators for complex numbers. there will be yet
; another table storing the right procedures for a specific combination of operation and
; type of number! this means that there will necessarily be another level of tagging to
; distinguish rational numbers, complex numbers and regular numbers...
; ---
(define (make-complex z)
  (attach-type 'complex z))
; ---
(define (+complex z1 z2)
  (make-complex (+c z1 z2)))
; ---
(define (-complex z1 z2)
  (make-complex (-c z1 z2)))
; ---
(define (*complex z1 z2)
  (make-complex (*c z1 z2)))
; ---
(define (/complex z1 z2)
  (make-complex (/c z1 z2)))

;------------------------------------------------------------------------------------------
; RATIONAL NUMBERS
;------------------------------------------------------------------------------------------

; rational number constructor and selectors
; (only the constructor have to be changed relative to the original implementation in
; order for rational numbers to be accomodated in the new package)
; ---
(define (make-rat n d)
  ; defining 'gcd'
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  ; constructing a rational number
  (if (< d 0)
      (make-rat (- n) (- d))
      (let ((g (gcd (abs n) d)))
        (attach-type 'rational
                     (cons (/ n g) (/ d g))))))
; ---
(define (numer r)
  (car r))
; ---
(define (denom r)
  (cdr r))

; operations on rational numbers ('-rat' and '/rat' implementations are slightly distinct
; from the original ones only because they are implemented in terms of '+rat' and '*rat',
; respectively)
; ---
(define (+rat r1 r2)
  (make-rat
   (+ (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
   (* (denom r1) (denom r2))))
; ---
(define (-rat r1 r2)
  (+rat
   r1
   (contents (make-rat (- (numer r2)) (denom r2)))))
; ---
(define (*rat r1 r2)
  (make-rat
   (* (numer r1) (numer r2))
   (* (denom r1) (denom r2))))
; ---
(define (/rat r1 r2)
  (*rat
   r1
   (contents (make-rat (denom r2) (numer r2)))))

;------------------------------------------------------------------------------------------
; REGULAR NUMBERS
;------------------------------------------------------------------------------------------

; constructor and operations
; ---
(define (make-number n)
  (attach-type 'number n))
; ---
(define (+number x y)
  (make-number (+ x y)))
; ---
(define (-number x y)
  (make-number (- x y)))
; ---
(define (*number x y)
  (make-number (* x y)))
; ---
(define (/number x y)
  (make-number (/ x y)))

;------------------------------------------------------------------------------------------
; POLYNOMIALS
;------------------------------------------------------------------------------------------

; a polynomial is represented by a variable (symbol) and an ordered (decreasing) list of
; terms (each term is a pair composed of its order and coefficient). for example, the
; polynomial x^15 + 2x^7 + 5 is represented by the symbol 'x and a list of terms given by
; ((15 1) (7 2) (0 5))
; ---
(define (make-polynomial var term-list)
  (attach-type 'polynomial (cons var term-list)))
; ---
(define (var p)
  (car p))
; ---
(define (term-list p)
  (cdr p))

; other useful predicates, constructors and selectors for polynomials
; ---
(define (same-var? v1 v2)
  (eq? v1 v2))
; ---
(define (empty-term-list? term-list)
  (null? term-list))
; ---
(define (adjoin-term x term-list)
  (cons x term-list))
; ---
(define (make-term order coeff)
  (cons order coeff))
; ---
(define (first-term term-list)
  (car term-list))
; ---
(define (rest-of-terms term-list)
  (cdr term-list))
; ---
(define (order term)
  (car term))
; ---
(define (coefficient term)
  (cdr term))

; operations on polynomials (for now, only addition and subtraction)
; ---
(define (+poly p1 p2)
  (if (eq? (var p1) (var p2))
      (make-polynomial
       (var p1)
       (op-terms +
                 (term-list p1)
                 (term-list p2)))
      (error "polynomials not in same variable")))
; ---
(define (-poly p1 p2)
  (if (eq? (var p1) (var p2))
      (make-polynomial
       (var p1)
       (op-terms -
                 (term-list p1)
                 (term-list p2)))
      (error "polynomials not in same variable")))
; ---
(define (op-terms OP L1 L2)
  (cond ((empty-term-list? L1) L2)
        ((empty-term-list? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1
                               (op-terms OP (rest-of-terms L1) L2)))
                 ((> (order t2) (order t1))
                  (adjoin-term t2
                               (op-terms OP L1 (rest-of-terms L2))))
                 (else
                  (adjoin-term (make-term (order t1)
                                          (OP (coefficient t1) (coefficient t2)))
                               (op-terms OP
                                         (rest-of-terms L1)
                                         (rest-of-terms L2)))))))))

; "Really there is only one interesting symbol in this procedure, only one interesting
; idea. The interesting idea is the 'ADD' [and 'SUB'] [would be placed as first arguments
; of 'op-terms' in '+poly' and '-poly' instead of '+' and '-', respectively]. And the
; reason that's interesting is because something complety wonderful just happened. We
; reduced adding polynomials, not to sort of plus [and minus], but to the generic 'ADD'
; [and 'SUB']! In other words, by implementing it that way, not only do we have a system
; in which we have rational numbers, complex numbers, ordinary numbers and polynomials,
; but the coefficients of the polynomials can be anything that the system can add [and
; subtract]! So, these can be polynomials whose coefficients are rational numbers, or
; complex numbers (which, in turn, can be either rectangular or polar), or ordinary
; numbers [or even other polynomials]."

;------------------------------------------------------------------------------------------
; GENERIC OPERATIONS 'ADD', 'SUB', 'MUL' AND 'DIV'
;------------------------------------------------------------------------------------------

; "What's going on is that there are these chains of types. And the length of the chain
; is sort of the number of levels that we go up in this table [the length of the chain of
; types is equal to the number of abstration barriers between the lowest and highest
; levels]. And what a type tells you, every time you have a vertical barrier in this
; table [for example, a barrier between rational, complex and regular numbers], where
; there's some ambiguity about where you should go down to the next level, the type is
; telling where to go. And then everybody at the bottom [at the lowest level], as they
; construct data and filter it up, they stick their type back on. That's the general
; structure of the system. So what I mean precisely is that our system right now
; automatically can handle things like, for example, adding together polynomials whose
; coefficients are rational numbers. That's merely because, or profoundly because, we
; reduced adding polynomials to adding their coefficients. And adding coefficients was
; done by the generic 'ADD' operator, which said 'I don´t care what your types are as
; long as I know how to add them.' So, automatically, for free, we get the ability to
; handle that. Well, that is even better than that, because remember one of the things we
; did is we put into the table that the way you add polynomials is to use '+poly'. That
; means that polynomials themselves are things that can be added [that is, the
; coefficients of a polynomial may be polynomials themselves; for example, the polynomial
; (x^2+1)y^2 + (x^3+2x)y + (x^4-7), a polynomial in y whose coefficients are polynomials
; in x, is allowed under our representation]. [This idea can be extended to rational and
; complex numbers. We just have to replace the symbols +,-,*,/ by the generic operators
; ADD, SUB, MUL, DIV in the operations regarding specific types of numbers; for example,
; '+c' would be written as
; ---
; (define (+c z1 z2)
;   (make-rectangular
;    (ADD (real-part z1) (real-part z2))
;    (ADD (imag-part z1) (imag-part z2))))
; ---
; and all of a sudden we may have complex numbers whose real and imaginary parts can be
; rational, complex or regular numbers, or even polynomials.] What's really going on
; here? What's really going on is that we got rid of this manager who's sitting there
; poking his nose into what everybody's business is, and built a system that has
; DECENTRALIZED CONTROL."

; construct table of generic operations
; (installing operators for complex numbers, rational numbers, ordinary numbers and
; polynomials - only addition and subtraction)
; ---
(define ops-table-init (empty-table))
; ---
(define ops-table-1
  (put ops-table-init 'complex 'add +complex))
; ---
(define ops-table-2
  (put ops-table-1 'complex 'sub -complex))
; ---
(define ops-table-3
  (put ops-table-2 'complex 'mul *complex))
; ---
(define ops-table-4
  (put ops-table-3 'complex 'div /complex))
; ---
(define ops-table-5
  (put ops-table-4 'rational 'add +rat))
; ---
(define ops-table-6
  (put ops-table-5 'rational 'sub -rat))
; ---
(define ops-table-7
  (put ops-table-6 'rational 'mul *rat))
; ---
(define ops-table-8
  (put ops-table-7 'rational 'div /rat))
; ---
(define ops-table-9
  (put ops-table-8 'number 'add +number))
; ---
(define ops-table-10
  (put ops-table-9 'number 'sub -number))
; ---
(define ops-table-11
  (put ops-table-10 'number 'mul *number))
; ---
(define ops-table-12
  (put ops-table-11 'number 'div /number))
; ---
(define ops-table-13
  (put ops-table-12 'polynomial 'add +poly))
; ---
(define ops-table
  (put ops-table-13 'polynomial 'sub -poly))

; defining generic operations in terms of 'operate-2'
; ---
(define (ADD x y)
  (operate-2 ops-table 'add x y))
; ---
(define (SUB x y)
  (operate-2 ops-table 'sub x y))
; ---
(define (MUL x y)
  (operate-2 ops-table 'mul x y))
; ---
(define (DIV x y)
  (operate-2 ops-table 'div x y))

;------------------------------------------------------------------------------------------
; TESTS
;------------------------------------------------------------------------------------------

(display "complex numbers") (newline) (display "---") (newline)
(define z1 (make-complex (make-rectangular 1 2)))
(define z2 (make-complex (make-polar 2 3)))
(ADD z1 z2)
(SUB z1 z2)
(MUL z1 z2)
(DIV z1 z2)
;-
; (DIV z1 z2)
; (operate-2 ops-table 'div z1 z2)
; ((get ops-table (type z1) 'div) (contents z1) (contents z2))
; ((get ops-table 'complex 'div) (contents z1) (contents z2))
; (/complex (contents z1) (contents z2))
; (/complex (rectangular 1 . 2) (polar 2 . 3))
; ---
(newline) (display "rational numbers") (newline) (display "---") (newline)
(define r1 (make-rat 1 2))
(define r2 (make-rat 3 4))
(ADD r1 r2)
(SUB r1 r2)
(MUL r1 r2)
(DIV r1 r2)
;-
; (DIV r1 r2)
; (operate-2 ops-table 'div r1 r2)
; ((get ops-table (type r1) 'div) (contents r1) (contents r2))
; ((get ops-table 'rational 'div) (contents r1) (contents r2))
; (/rat (contents r1) (contents r2))
; (/rat (1 . 2) (3 . 4))
; ---
(newline) (display "regular numbers") (newline) (display "---") (newline)
(define n1 (make-number 2))
(define n2 (make-number 4))
(ADD n1 n2)
(SUB n1 n2)
(MUL n1 n2)
(DIV n1 n2)
;-
; (DIV n1 n2)
; (operate-2 ops-table 'div n1 n2)
; ((get ops-table (type n1) 'div) (contents n1) (contents n2))
; ((get ops-table 'number 'div) (contents n1) (contents n2))
; (/number (contents n1) (contents n2))
; (/number (1 . 2) (3 . 4))
; ---
(newline) (display "polynomials") (newline) (display "---") (newline)
;-
(define (display-poly p)
  (let ((poly (contents p)))
    (let ((t (first-term (term-list poly))))
      (display (coefficient t))
      (display "*")
      (display (var poly))
      (display "^")
      (display (order t))
      (cond ((not (empty-term-list? (rest-of-terms (term-list poly))))
             (display " + ")
             (display-poly (make-polynomial
                            (var poly)
                            (rest-of-terms (term-list poly)))))
            (else
             (newline))))))
;-
(define t1 (list (make-term 15 1) (make-term 7 2) (make-term 0 5)))
(define p1 (make-polynomial 'x t1))
(define t2 (list (make-term 12 4) (make-term 7 3) (make-term 1 1) (make-term 0 1)))
(define p2 (make-polynomial 'x t2))
;-
p1
(display-poly p1)
p2
(display-poly p2)
(ADD p1 p2)
(display-poly (ADD p1 p2))
(SUB p1 p2)
(display-poly (SUB p1 p2))
;-
; (SUB p1 p2)
; (operate-2 ops-table 'sub p1 p2)
; ((get ops-table (type p1) 'sub) (contents p1) (contents p2))
; ((get ops-table 'polynomial 'sub) (contents p1) (contents p2))
; (-poly (contents p1) (contents p2))
; (-poly
;  (cons 'x (list (make-term 15 1) (make-term 7 2) (make-term 0 5)))
;  (cons 'x (list (make-term 12 4) (make-term 7 3) (make-term 1 1) (make-term 0 1))))

