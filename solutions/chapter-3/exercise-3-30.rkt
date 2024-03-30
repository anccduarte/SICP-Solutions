
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.30.
;---
;Figure 3.27 shows a ripple-carry adder formed by stringing together n full-adders. This
;is the simplest form of parallel adder for adding two n-bit binary numbers. The inputs
;A1, A2, A3, ..., An and B1, B2, B3, ..., Bn are the two binary numbers to be added (each
;Ak and Bk is a 0 ora 1). The circuit generates S1, S2, S3, ..., Sn , the n bits of the
;sum, and C, the carry from the addition. Write a procedure 'ripple-carry-adder' that
;generates this circuit. The procedure should take as arguments three lists of n wires
;each — the Ak , the Bk , and the Sk — and also another wire C. The major drawback of the
;ripple-carry adder is the need to wait for the carry signals to propagate. What is the
;delay needed to obtain the complete output from an n-bit ripple-carry adder, expressed
;in terms of the delays for and-gates, or-gates, and inverters?
;------------------------------------------------------------------------------------------

;ripple-carry adder schematization
;---
;                  C1                  C2                  C3 | |                C(n)=0
;      A1   B1   +----+    A2   B2   +----+    A3   B3   +----| |    An   Bn   +---- 
;      |    |    |    |    |    |    |    |    |    |    |    | |    |    |    |
;    +-------------+  |  +-------------+  |  +-------------+  | |  +-------------+
;    | full-adder  |  |  | full-adder  |  |  | full-adder  |  | |  | full-adder  |
;    +-------------+  |  +-------------+  |  +-------------+  | |  +-------------+
;      |         |    |    |         |    |    |              | |    |         |
; C ---+         S1   +----+         S2   +----+              | |----+         Sn
;                                                             | | C(n-1)

;'ripple-carry-adder'
;---
(define (ripple-carry-adder A B C-in S)
  (if (null? A)
      'ok
      (let ((C-out (make-wire)))
        (full-adder (car A) (car B) C-in (car S) C-out)
        (ripple-carry-adder (cdr A) (cdr B) C-out (cdr S)))))

