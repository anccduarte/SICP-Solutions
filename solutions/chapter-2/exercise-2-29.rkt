
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 2.29.
;---
;A binary mobile consists of two branches, a left branch and a right branch. Each branch
;is a rod of a certain length, from which hangs either a weight or another binary mobile.
;We can represent a binary mobile using compound data by constructing it from two
;branches (for example, using 'list') (SEE BELOW *). A branch is constructed from a
;'length' (which must be a number) together with a 'structure', which may be either a
;number (representing a simple weight) or another mobile (SEE BELOW **).
;---
;(a) Write the corresponding selectors 'left-branch' and 'right-branch', which return the
;branches of a mobile, and 'branch-length' and 'branch-structure', which return the
;components of a branch.
;---
;(b) Using your selectors, define a procedure 'total-weight' that returns the total
;weight of a mobile.
;---
;(c) A mobile is said to be balanced if the torque applied by its top-left branch is
;equal to that applied by its top-right branch (that is, if the length of the left rod
;multiplied by the weight hanging from that rod is equal to the corresponding product for
;the right side) and if each of the submobiles hanging off its branches is balanced.
;Design a predicate that tests whether a binary mobile is balanced.
;---
;(d) Suppose we change the representation of mobiles so that the constructors are (SEE
;BELOW ***). How much do you need to change your programs to convert to the new
;representation?
;------------------------------------------------------------------------------------------


;(*) 'make-mobile'
;---
(define (make-mobile left right)
  (list left right))

;(**) 'make-branch'
;---
(define (make-branch length structure)
  (list length structure))


;------------------------------------------------------------------------------------------
;PART A
;------------------------------------------------------------------------------------------

;'mobile' selectors
;---
(define (left-branch mobile)
  (car mobile))
;---
(define (right-branch mobile)
  (car (cdr mobile)))

;'branch' selectors
;---
(define (branch-length branch)
  (car branch))
;---
(define (branch-structure branch)
  (car (cdr branch)))


;------------------------------------------------------------------------------------------
;PART B
;------------------------------------------------------------------------------------------

;tree representation of a mobile
;(example -> left branch has a weight (w) 'structure' and right branch has a mobile (m)
;'structure' (m); both branches of the latter 'structure' are composed of a weight
;structure)
;---
;            mobile
;          /        \
;    length          length
;      |                |
; (w)structure      structure(m)
;                    /       \
;                 length   length
;                 |             |
;           (w)structure   structure(w)

;'total-weight' -> sum of the 'structure' nodes that are leaves
;---
(define (total-weight mobile)
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))))

;test for random mobiles
;---
(display "PART B") (newline)
;---
(define mobile1 (make-mobile (make-branch 1 2)
                             (make-branch 3 4)))
(total-weight mobile1)
;---
(define mobile2 (make-mobile (make-branch 1
                                          (make-mobile (make-branch 5 6)
                                                       (make-branch 7 8)))
                             (make-branch 3 4)))
(total-weight mobile2)


;------------------------------------------------------------------------------------------
;PART C
;------------------------------------------------------------------------------------------

;version 1 -> 'balanced?-1' (more convoluted)
;---
(define (convert bool) (if (equal? bool #t) 1 0))
(define (odd? n) (not (= (remainder n 2) 0)))
;---
(define (balanced?-1 mobile)
  (let ((llen (branch-length (left-branch mobile)))
        (rlen (branch-length (right-branch mobile)))
        (lstruct (branch-structure (left-branch mobile)))
        (rstruct (branch-structure (right-branch mobile))))
    (if (odd? (+ (convert (pair? lstruct))
                 (convert (pair? rstruct))))
        false
        (let ((eq-weight? (= (* llen (total-weight lstruct))
                             (* rlen (total-weight rstruct)))))
          (if (not (pair? lstruct))
              eq-weight?
              (and eq-weight?
                   (balanced?-1 lstruct)
                   (balanced?-1 rstruct)))))))

;version 2 -> 'balanced?-2' (better)
;inspired by Rather Iffy's and 2DSharp's solutions
;http://community.schemewiki.org/?sicp-ex-2.29
;---
(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))
;---
(define (balanced?-2 mobile)
  (if (not (pair? mobile))
      true
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced?-2 (branch-structure (left-branch mobile)))
           (balanced?-2 (branch-structure (right-branch mobile))))))

;test both versions for random mobiles
;---
(display "---") (newline) (display "PART C") (newline)
;---
(define (test-balanced mobile)
  (display "balanced?-1 -> ") (display (balanced?-1 mobile)) (newline)
  (display "balanced?-2 -> ") (display (balanced?-2 mobile)) (newline))
;---
(test-balanced mobile1)
(test-balanced mobile2)
;---
(define mobile3 (make-mobile (make-branch 3 4)
                             (make-branch 2 6)))
(test-balanced mobile3)
;---
(define mobile4 (make-mobile (make-branch 2 (make-mobile (make-branch 3 4)
                                                         (make-branch 2 6)))
                             (make-branch 4 (make-mobile (make-branch 3 2)
                                                         (make-branch 2 3)))))
(test-balanced mobile4)


;------------------------------------------------------------------------------------------
;PART D
;------------------------------------------------------------------------------------------

;(***) new constructors
;---
(define (make-mobile-new left right)
  (cons left right))
;---
(define (make-branch-new length structure)
  (cons length structure))

;given this representation, we must only modify 2 out of the 4 selectors:
;- 'right-branch' is set to (cdr mobile) instead of (car (cdr mobile))
;- 'branch-structure' is set to (cdr branch) instead of (car (cdr branch))
;---
(define (right-branch-new mobile)
  (cdr mobile))
;---
(define (branch-structure-new branch)
  (cdr branch))

