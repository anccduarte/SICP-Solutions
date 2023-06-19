
#lang sicp


;------------------------------------------------------------------------------------------
;RATIONAL NUMBERS
;---
;Module implementing basic rational number operations. A rational number is defined as
;(Rational <num> <denom>) and the following operations may be performed (assuming that
;'r1' and 'r2' are rational numbers):
;---
;1. get numerator -> ((r1 "get-num"))
;2. get denominator -> ((r1 "get-denom"))
;3. display the rational -> ((r1 "display-rat"))
;4. add two rationals -> ((r1 "add-rat") r2)
;5. multiply two rationals -> ((r1 "mul-rat") r2)
;6. subtract two rationals -> ((r1 "sub-rat") r2)
;7. divide two rationals -> ((r1 "div-rat") r2)
;8. check for equality -> ((r1 "equal-rat?") r2)
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;IMPLEMENTATION
;------------------------------------------------------------------------------------------

;'Rational' -> returns a procedure waiting to be called; the
;action to be performed depends on the message passed to the
;procedure returned by 'Rational'
;---
(define (Rational num denom)

  ;helper procedure computing the greatest common divisor of
  ;two integers
  ;---
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

  ;helper procedure that constructs a rational number whose
  ;numerator and denominator are reduced to lowest terms
  ;---
  (define (make-rat n d)
    (if (< d 0)
        (make-rat (- n) (- d))
        (let ((g (abs (gcd n d))))
          (Rational (/ n g) (/ d g)))))

  ;procedure to be returned by 'Rational' -> the action to
  ;be performed by 'dispatch' depends on its input
  ;---
  (define (dispatch message)
    
          ;get numerator of the "current" rational
    (cond ((equal? message "get-numer")
           (lambda () num))
          
          ;get denominator of the "current" rational
          ((equal? message "get-denom")
           (lambda () denom))
          
          ;display "current" rational number
          ((equal? message "display-rat")
           (lambda ()
             (let ((r (make-rat num denom)))
               (display ((r "get-numer")))
               (display "/")
               (display ((r "get-denom")))
               (newline))))
          
          ;add "current" rational and <r2>
          ((equal? message "add-rat")
           (lambda (r2)
             (let ((n2 ((r2 "get-numer")))
                   (d2 ((r2 "get-denom"))))
               (let ((n-res (+ (* num d2)
                               (* n2 denom)))
                     (d-res (* denom d2)))
                 (make-rat n-res d-res)))))
          
          ;multiply "current" rational by <r2>
          ((equal? message "mul-rat")
           (lambda (r2)
             (let ((n2 ((r2 "get-numer")))
                   (d2 ((r2 "get-denom"))))
               (let ((n-res (* num n2))
                     (d-res (* denom d2)))
                 (make-rat n-res d-res)))))
          
          ;subtract <r2> to the "current" rational
          ((equal? message "sub-rat")
           (lambda (r2)
             (let ((n2 (- ((r2 "get-numer"))))
                   (d2 ((r2 "get-denom"))))
               (let ((r-new (Rational num denom))
                     (r2-new (Rational n2 d2)))
                 ((r-new "add-rat") r2-new)))))
          
          ;divide "current" rational by <r2>
          ((equal? message "div-rat")
           (lambda (r2)
             (let ((n2 ((r2 "get-numer")))
                   (d2 ((r2 "get-denom"))))
               (let ((n-res (* num d2))
                     (d-res (* n2 denom)))
                 (make-rat n-res d-res)))))

          ;check whether "current" and <r2> are equal
          ((equal? message "equal-rat?")
           (lambda (r2)
             (let ((n2 ((r2 "get-numer")))
                   (d2 ((r2 "get-denom"))))
               (= (* num d2)
                  (* n2 denom)))))
          (else
           (error "Not implemented!"))))

  ;return 'dispatch' ready to be called
  dispatch)


;------------------------------------------------------------------------------------------
;SIMPLE TESTS
;------------------------------------------------------------------------------------------

;define two rational numbers
(define r1 (Rational 1 2))
(define r2 (Rational 3 4))

;add two rationals (<r1> + <r2>)
(define r-add ((r1 "add-rat") r2))
;display <r-add> (the result of <r1> + <r2>)
(display "(+) ")((r-add "display-rat"))

;multiply two rationals (<r1> * <r2>)
(define r-mul ((r1 "mul-rat") r2))
;display <r-mul> (the result of <r1> * <r2>)
(display "(*) ") ((r-mul "display-rat"))

;subtract two rationals (<r1> - <r2>)
(define r-sub ((r1 "sub-rat") r2))
;display <r-sub> (the result of <r1> - <r2>)
(display "(-) ") ((r-sub "display-rat"))

;divide two rationals (<r1> / <r2>)
(define r-div ((r1 "div-rat") r2))
;display <r-div> (the result of <r1> / <r2>)
(display "(/) ") ((r-div "display-rat"))

;define new rational
(define r3 (Rational 8 12))
;check whether <r3> and <r-div> are equal
(display "(=?) ") ((r-div "equal-rat?") r3)

