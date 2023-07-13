
#lang sicp

;------------------------------------------------------------------------------------------
; Lecture 4A: Pattern Matching and Rule-based Substitution
; (note: many predicates and selectors are omitted in the present implementation of a
; rule-based substitution system; nevertheless, the ideas and general patterns of the
; program are still perfectly captured)
;------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------
; Part 1
;------------------------------------------------------------------------------------------

; general view of rule-based substitution
; ---
;                Rule
; Pattern -----------------> Skeleton
;    |                          |
;    | Match                    | Instantiation
;    |                          |
;    v                          v
; Expression --------------> Expression
;  (source)                   (target)
; ---
; a given pattern "produces" a skeleton. if an expression matches the pattern, that
; expression is substituted by another expression via instantiation of the skeleton

; rules for pattern matching
; ---
; 'foo -------- matches exactly 'foo
; (f a b) ----- matches a list whose first element is 'f
;                                    second   "    is 'a
;                                    third    "    is 'b
; (? x) ------- matches anything, call it x
; (?c x) ------ matches a constant, call it x
; (?v x) ------ matches a variable, call it x

; skeletons for instantiation
; ---
; 'foo -------- instantiates to itself
; (f a b) ----- instantiates to a 3-list that is the result of instantiating f, a and b
; (: x) ------- instantiates to the value of x in the pattern matched 

; more detailed scheme for rule-based substitution
; ---
;                               +-------------+
;                             +-------------+ | (**)
;                           +-------------+ |-+
;                           |     rule    |-+
;                           +------+------+
;              +----------- | pat  | ske  | -----------+
;              |   (+)      +------+------+    (++)    |
;              |                                       |
;              v                                       v
;        +-------------+     dictionary (*)     +-------------+
;        |    match    | ---------------------> | instantiate |
;        +-------------+                        +-------------+
;              ^                                       |
;              |            expressions (***)          |
;              +---------------------------------------+ 
; ---
; (*)   dictionary storing information on the values of the pattern variables matched
;       (a dictionary is no more than a mapping of pattern variables to the values that
;       were found by matching the expression and the pattern)
; (**)  set of rules to be tried for every subexpression of the original expression
; (***) every time an expression is matched, a new equivalent expression is constructed
;       via instantiation; this expression is then fed again to the matcher and all the
;       rules are subsequently tried
; ---
; (+)   for each rule, the pattern component is fed to the matcher
; (++)  for each rule, the skeleton component is fed to the instantiator
; ---
; "You can think of this as sort of an organic process. You've got some sort of stew,
; right? You've got bacteria or enzymes is some gooey mess. And these enzymes change
; things. They attach to your expression, change it, and then they go away. They match,
; they change and they go away. You can imagine it as a parallel process of some sort.
; So, you stick an expression into this mess, and after a while you take it out and it
; has been simplified. And it just keeps changing until it no longer can be changed."

;------------------------------------------------------------------------------------------
; Part 2
;------------------------------------------------------------------------------------------

; general structure of the 'matcher'
; ---
;                        pattern
;                           |
;                           v                (*)
;                  +----------------+    dictionary
;    expression    |                |    (augmented)
; ---------------> |     matcher    | ---------------->
;                  |                |
;                  +----------------+
;                           ^
;                           |
;                       dictionary
;                   (initially empty)
; ---
; (*) dictionary that maps each pattern variable to the value found by matching the
;     pattern and the expression

; scheme procedure for the matcher
; (note that, in the general case, the dictionary passed to the matcher is itself the
; result of calling the matcher -> the dictionary results from matching the 'car' of the
; pattern and the 'car' of the expression; next the 'cdrs' of the pattern and the
; expression are analyzed using the updated dictionary)
; ---
(define (matcher pat exp dict)
        ;match failed at the previous subexpression (because the
        ;matcher takes the dictionary from the previous matcher,
        ;it must be able to propagate the errors - if the match of
        ;the 'cars' fails, the failure is propagated to the match
        ;of the 'cdrs' and the process is immediately stoped)
  (cond ((eq? dict 'failed) 'failed)
        ;the pattern is just a symbol -> (not (pair? pat))
        ;this catches things like operators (+,*,...) - note that
        ;nothing is added to the dictionary
        ((atom? pat)
         (if (atom? exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        ;the expression is atomic, but the pattern is not
        ((atom? exp) 'failed)
        ;cases involving pattern constants -> (?c x)
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))
        ;cases involving pattern variables -> (?v x)
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))
        ;cases involving other expressions -> (? x)
        ((arbitrary-expression? pat)
         (extend-dict pat exp dict))
        ;general case (this is where the expression and the pattern
        ;are broken into subexpressions - the tree of the expression
        ;and the tree of the pattern are analyzed simultaneously!)
        (else
         (matcher (cdr pat)
                  (cdr exp)
                  (matcher (car pat)
                           (car exp)
                           dict)))))

; general structure of the 'instantiater'
; (note that, what is happening is that for every rule, it's pattern component is fed to
; the matcher and it's skeleton component is fed to the instantiator via some kind of
; selector procedures for rules; however, we only need the skeleton part whenever we have
; a match, that is, on average, the instantiater is called a lot less times than the
; matcher - in the worst case scenario, the matcher is called for all possible
; combinations rule+subexpression)
; ---
;                        skeleton
;                           |
;                           v
;                  +----------------+       new
;    dictionary    |                |    expression
; ---------------> |  instantiater  | ---------------->
;                  |                |
;                  +----------------+

; scheme procedure for the instantiater
; ---
(define (instantiater skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ;a skeleton evaluation is an expression like (: x)
          ;'eval-exp' is just an abstraction for 'cadr'
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else
           (cons (loop (car s))
                 (loop (cdr s))))))
  (loop skeleton))

; 'evaluate' -> helper procedure (used in the second condition of the instantiater)
; (right now it's really supposed to seem like magic - 'apply', 'eval', 'mapcar',
; 'user-initial-environment' ???)
; ---
(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (mapcar (lambda (v)
                 (lookup v dict)
                 (cdr form))))))

;------------------------------------------------------------------------------------------
; Part 3
;------------------------------------------------------------------------------------------

; "Now we have to understand the control structure by which the rules are applied to the
; expressions so as to do algebraic simplification. That's also a big complicated mess.
; The problem is that there is a variety of interlocking, interwoven loops, if you will,
; involved in this. For one thing, I have to examine every subexpression of my expression
; that I'm trying to simplify. That we know how to do. Is a 'car cdr' recursion of some
; sort, some sort of tree walk. And that's going to be happening. Now, for every such
; place, every node that I get to in doing my traversal of the expression I'm trying to
; simplify, I want to apply all of the rules. Every rule is going to look at every node.
; I'm going to rotate the rules around. Now, either a rule will or will not match. If the
; rule does not match, then it's not very interesting. If the rule does match, then I'm
; going to replace that node in the expression by an alternate expression. I'm actually
; going to make a new expression resulting from instantiating the skeleton for that rule.
; But no one knows whether that thing that I instantiated there is in simplified form.
; So we're going to have to simplify that, somehow we have to call the simplifier on the
; thing that I just constructed. And then when that's done, then I sort of can build that
; into the expression I want as my answer. Now, there is a basic idea here, which I will
; call a garbage-in, garbage-out (GIGO) simplifier. It's a kind of recursive simplifier."

; [On the very convoluted recursive patterns that sometimes arise in a program, just like
; this one] "The key to good programming and good design is to know what not to think
; about. You have to learn how to program that way - with abandon."

; scheme procedure for the simplifier (where the matcher and the instantiater meet)
; (note that 'simplifier' is a procedure that takes an argument 'the-rules', which is
; simply a quoted list of rules, each one having a pattern component and a skeleton
; component, and returns an internally defined procedure 'simplify-exp' that takes an
; expression to be simplified as a single argument)
; ---
(define (simplifier the-rules)
  ;traversing the expression ('simplify-exp' + 'simplify-parts')
  ;this could be written as a single procedure (see the resemblances
  ;between 'simplify-parts' and 'map'?)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp) ;(map simplify-exp exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))
  ;trying the rules (note the selectors for patterns and skeletons -
  ;- 'rule-pattern' and 'rule-skeleton', respectively)
  (define (try-rules exp)
    (define (scan rules)
      (if (nul? rules)
          '()
          (let ((dict
                 (matcher (rule-pattern (car rules))
                          exp
                          (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                ;substitute initial expression for another via
                ;instantiation of the skeleton that is part of the
                ;rule whose pattern produced a match, and further
                ;simplify the resulting expression
                (simplify-exp
                 (instantiater (rule-skeleton (car rules))
                               dict))))))
    (scan the-rules))
  simplify-exp)

; lastly, procedures on dictionaries used throughout the program
; ---
(define (empty-dictionary) '())
; ---
(define (extend-dictionary pat dat dict)
  ;'variable-name' is probably just an abstraction for 'cadr'
  (let ((name (variable-name pat)))
    ;given a name and a dictionary, 'assq' returns a pair name+value,
    ;name=(car pair) and value=(cadr pair), if the name is in the
    ;dictionary and '() otherwise
    (let ((v (assq name dict)))
      (cond ((null? v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))
; ---
(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))

;------------------------------------------------------------------------------------------
; Part 4
;------------------------------------------------------------------------------------------

; the 'deriv' program may now be written in terms of 'simplifier' and some set of rules
; for taking derivatives! 'deriv' is simply an instance of 'simplifier', such that it
; simplifies expressions based on a set of rules 'deriv-rules'. note that all rules in
; 'deriv-rules' have two parts - the first part is the pattern, i.e. the 'car', and the
; second part is the skeleton, i.e. the 'cadr' 
; ---
(define deriv (simplifier deriv-rules))
; ---
(define deriv-rules
  '(
    ;basic rules for atomic expressions
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)
    ;derivative rule for addition
    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))
    ;derivative rule for multiplication
    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1)
           (dd (: x2) (: v)))
        (* (dd (: x1) (: v))
           (: x2))))
    ;derivative rule for exponentiation
    ((dd (** (? x) (?c n)) (? v))
     (* (* (: n)
           (** (: x) (: (- n 1))))
        (dd (: x) (: v))))
    ))

; 'deriv' would be used as follows
; (in this particular case we compute the derivative of the expression 2x + y with
; respect to the variable x)
; ---
(define exp '(dd (+ (* 2 x) y) x))
(deriv exp) ;(+ 2 0)

