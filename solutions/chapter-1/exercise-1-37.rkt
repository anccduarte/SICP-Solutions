
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.37.
;---
;a. An infinite continued fraction is an expression of the form:
;---
;f =          N1
;    ------------------
;    D1 +      N2
;         -------------
;         D2 +    N3
;              --------
;              D3 + ...
;---
;As an example, one can show that the infinite continued fraction expansion with the 'Ni'
;and the 'Di' all equal to 1 produces 1/φ, where φ is the golden ratio. One way to
;approximate an infinite continued fraction is to truncate the expansion after a given
;number of terms. Such a truncation - a so-called k-term finite continued fraction — has
;the form:
;---
;      N1
; -------------
; D1 +    N2
;      --------
;      D2 +   N3
;       .   --------
;         . +   Nk
;           .  ----
;             . Dk
;---
;Suppose that 'n' and 'd' are procedures of one argument (the term index 'i') that return
;the 'Ni' and 'Di' of the terms of the continued fraction. Define a procedure 'cont-frac'
;such that evaluating (cont-frac n d k) computes the value of the k-term finite continued
;fraction. Check your procedure by approximating 1/φ using (SEE BELOW *) for successive
;values of 'k'. How large must you make 'k' in order to get an approximation that is
;accurate to 4 decimal places?
;---
;b. If your 'cont-frac' procedure generates a recursive process, write one that generates
;an iterative process. If it generates an iterative process, write one that generates a
;recursive process.
;------------------------------------------------------------------------------------------


;---
;PART A
;---

;defining 'cont-frac' -> recursive ('cont-frac-rec')
;---
(define (cont-frac-rec n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i)
              (iter (inc i))))))
  (iter 1))

;(*) approximating phi
;---
(define (approx-phi k)
  (let ((inv-phi (cont-frac-rec (lambda (i) 1.0)
                                (lambda (i) 1.0)
                                k)))
    (/ 1 inv-phi)))

;test for some values of k
;---
(define (display-iter i)
    (display "k = ") (display i)
    (display " => ")
    (display "phi = ") (display (approx-phi i))
    (newline))
;---
(define (test k)
  (define (iter i)
    (cond ((> i k)
           (display ""))
          (else
           (display-iter i)
           (iter (inc i)))))
  (iter 1))
;---
(display "PART A") (newline) (display "---") (newline)
(test 20)

;conclusion
;---
;φ = 1.6180 (up to 4 decimal places)
;k = 11 => φ = 1.6179775280898876 ~= 1.6180
;hence, k must be at least 11 to get an approximation accurate to 4 decimal places


;---
;PART B
;---

;redefining 'cont-frac' -> iterative ('cont-frac-iter')
;---
(define (cont-frac-iter n d k)
  (define (iter i res)
    (if (> i k)
        res
        (iter (inc i)
              (/ (n i)
                 (+ (d i) res)))))
  (iter 1 0))


;compare implementations of 'cont-frac'
;---
(newline) (display "PART B") (newline) (display "---") (newline)
;---
(define (approx-phi-2 k)
  (let ((inv-phi (cont-frac-iter (lambda (i) 1.0)
                                 (lambda (i) 1.0)
                                 k)))
    (/ 1 inv-phi)))
;---
(display "k = 10 (recursive) -> ") (approx-phi 10)
(display "k = 10 (iterative) -> ") (approx-phi-2 10)
(display "k = 100 (recursive) -> ") (approx-phi 100)
(display "k = 100 (iterative) -> ") (approx-phi-2 100)

