
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.40.
;---
;Give all possible values of 'x' that can result from executing
;---
;(define x 10)
;(parallel-execute (lambda () (set! x (* x x)))
;                  (lambda () (set! x (* x x x))))
;---
;Which of these possibilities remain if we instead use serialized procedures:
;---
;(define x 10)
;(define s (make-serializer))
;(parallel-execute (s (lambda () (set! x (* x x))))
;                  (s (lambda () (set! x (* x x x)))))
;------------------------------------------------------------------------------------------

;computing all possible outcomes assuming the procedures are not serialized
;[from the listing below, we may infer that the possible outcomes are restricted to 100,
;1.000, 10.000, 100.000 and 1.000.000; note that serializing the procedures reduces the
;list of possible combinations to the first two sequences of steps below, that is, we may
;only attain a single outcome: 1.000.000; for a more elegant solution to the problem, see
;Hoonji's answer at http://community.schemewiki.org/?sicp-ex-3.40]
;---
;P1: (lambda () (set! x (* x x)))
;P2: (lambda () (set! x (* x x x)))
;---
;1. P1 accesses 'x' twice (x=10)
;2. P1 sets 'x' to 100
;3. P2 accesses 'x' thrice (x=100, x=100, x=100)
;4. P2 sets 'x' to 1.000.000 (100*100*100)
;---
;1. P2 accesses 'x' thrice (x=10, x=10, x=10)
;2. P2 sets 'x' to 1.000
;3. P1 accesses 'x' twice (x=1.000, x=1.000)
;4. P1 sets 'x' to 1.000.000 (1.000*1.000)
;---
;1. P1 accesses 'x' twice (x=10)
;2. P2 accesses 'x' thrice (x=10, x=10, x=10)
;3. P1 sets 'x' to 100
;4. P2 sets 'x' to 1.000 (10*10*10)
;---
;1. P1 accesses 'x' twice (x=10)
;2. P2 accesses 'x' thrice (x=10, x=10, x=10)
;3. P2 sets 'x' to 1.000
;4. P1 sets 'x' to 100 (10*10)
;---
;1. P1 accesses "first" 'x' (x=10)
;2. P2 accesses 'x' thrice (x=10, x=10, x=10)
;3. P2 sets 'x' to 1.000
;4. P1 accesses "second" 'x' (x=1.000)
;5. P1 sets 'x' to 10.000 (10*1.000)
;---
;1. P2 accesses "first" 'x' (x=10)
;2. P1 accesses 'x' twice (x=10, x=10)
;3. P1 sets 'x' to 100
;4. P2 accesses "second" and "third" 'x' (x=100, x=100)
;5. P2 sets 'x' to 100.000 (10*100*100)
;---
;1. P2 accesses "first" and "second" 'x' (x=10, x=10)
;2. P1 accesses 'x' twice (x=10, x=10)
;3. P2 sets 'x' to 100
;4. P2 accesses "third" 'x' (x=100)
;5. P2 sets 'x' to 10.000 (10*10*100)

