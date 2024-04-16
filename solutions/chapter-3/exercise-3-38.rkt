
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.38.
;---
;Suppose that Peter, Paul, and Mary share a joint bank account that initially contains
;$100. Concurrently, Peter deposits $10, Paul withdraws $20, and Mary withdraws half the
;money in the account, by executing the following commands:
;---
;Peter: (set! balance (+ balance 10))
;Paul: (set! balance (- balance 20))
;Mary: (set! balance (- balance (/ balance 2)))
;---
;(a) List all the different possible values for balance after these three transactions
;have been completed, assuming that the banking system forces the three processes to run
;sequentially in some order.
;---
;(b) What are some other values that could be produced if the system allows the processes
;to be interleaved? Draw timing diagrams like the one in Figure 3.29 to explain how these
;values can occur.
;------------------------------------------------------------------------------------------

;(a) sequential execution may be carried out six distinct ways (exposed below); it is
;clear that sequential execution restricts the space of possible outcomes to a set of 4
;results: $35, $40, $45 and $50
;---
;1. Peter: $100 -> $110  |  1. Peter: $100 -> $110
;2. Paul:  $110 -> $90   |  2. Mary:  $110 -> $55
;3. Mary:  $90  -> $45   |  3. Paul:  $55  -> $35
;---
;1. Paul:  $100 -> $80   |  1. Paul:  $100 -> $80
;2. Peter: $80  -> $90   |  2. Mary:  $80  -> $40
;3. Mary:  $90  -> $45   |  3. Peter: $40  -> $50
;---
;1. Mary:  $100 -> $50   |  1. Mary:  $100 -> $50
;2. Peter: $50  -> $60   |  2. Paul:  $50  -> $30
;3. Paul:  $60  -> $40   |  3. Peter: $30  -> $40

;(b) two examples of alternative outcomes produced via interleaving individual processes
;are provided below; the first produces an outcome of $25 and the second results in a
;final balance of $55
;---
;  Peter              | Paul               | Mary
; --------------------+--------------------+--------------------
;                                                                 |
;                                            Access: $100         |
;  Access:  $100                                                  |
;  Deposit: $110                                                  |
;                                            Access:   $110       |
;                                            Withdraw: $45        |
;                       Access:   $45                             |
;                       Withdraw: $25                             |
;                                                                 ↓ time
;---
;  Peter              | Paul               | Mary
; --------------------+--------------------+--------------------
;                                                                 |
;                       Access: $100                              |
;  Access: $100                                                   |
;                       Withdraw: $80                             |
;  Deposit: $110                                                  |
;                                            Access:   $110       |
;                                            Withdraw: $55        |
;                                                                 ↓ time

