
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 2.43.
;---
;Louis Reasoner is having a terrible time doing Exercise 2.42. His 'queens' procedure
;seems to work, but it runs extremely slowly. (Louis never does manage to wait long
;enough for it to solve even the 6 x 6 case.) When Louis asks Eva Lu Ator for help, she
;points out that he has interchanged the order of the nested mappings in the 'flatmap',
;writing it as
;---
;(flatmap
; (lambda (new-row)
;   (map (lambda (rest-of-queens)
;          (adjoin-position new-row k rest-of-queens))
;        (queen-cols (- k 1))))
; (enumerate-interval 1 board-size))
;---
;Explain why this interchange makes the program run slowly. Estimate how long it will
;take Louis's program to solve the eight-queens puzzle, assuming that the program in
;Exercise 2.42 solves the puzzle in time T.
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;PART 1
;(how the original program works)
;------------------------------------------------------------------------------------------

;result of (queen-cols 0)
;---
;(())

;result of (queen-cols 1)
;(after flatmapping -> more detail in 'result of (queen-cols 2)')
;---
;((1 1) (2 1) ... (8 1))

;result of (queen-cols 2)
;---
;1.
;(1 1) -> 1st element of 'rest-of-queens' (first list in (queen-cols 1))
;(cons (list x 2) (1 1)) for x in [1, 8]
;(((1 2) (1 1)) ((2 2) (1 1)) ... ((8 2) (1 1)))
;---
;2.
;(2 1) -> 2nd element of 'rest-of-queens' (second list in (queen-cols 1))
;(cons (list x 2) (2 1)) for x in [1, 8]
;(((1 2) (2 1)) ((2 2) (2 1)) ... ((8 2) (2 1)))
;---
;...
;---
;8.
;(8 1) -> 8th element of 'rest-of-queens' (eighth list in (queen-cols 1))
;(cons (list x 2) (8 1)) for x in [1, 8]
;(((1 2) (8 1)) ((2 2) (8 1)) ... ((8 2) (8 1)))
;---
;final result for k=2 (after outer mapping)
;---
;((((1 2) (1 1)) ((2 2) (1 1)) ... ((8 2) (1 1)))
; (((1 2) (2 1)) ((2 2) (2 1)) ... ((8 2) (2 1)))
; ...
; (((1 2) (8 1)) ((2 2) (8 1)) ... ((8 2) (8 1))))
;---
;final result with flatmapping
;---
;(((1 2) (1 1)) ((2 2) (1 1)) ... ((8 2) (1 1))
; ((1 2) (2 1)) ((2 2) (2 1)) ... ((8 2) (2 1))
; ...
; ((1 2) (8 1)) ((2 2) (8 1)) ... ((8 2) (8 1)))
;---
;finally, filtering with 'safe?' (the filtering is performed by checking whether every
;newly added position is valid relative to all pre-existent combinations, that is, for
;every new position added to the board, it is checked whether it provokes any conflict
;with previous positions row-wise, column-wise and diagonal-wise)

;same reasoning applies for:
;---
;(queen-cols 3)
;(queen-cols 4)
;(queen-cols 5)
;(queen-cols 6)
;(queen-cols 7)
;(queen-cols 8)


;------------------------------------------------------------------------------------------
;PART 2
;(how Louis Reasoner's program works)
;------------------------------------------------------------------------------------------

;result of (queen-cols 0)
;---
;(())

;result of (queen-cols 1)
;(after flatmapping -> more detail in 'result of (queen-cols 2)')
;---
;((1 1) (2 1) ... (8 1))

;result of (queen-cols 2)
;---
;1.
;1 -> 1st x of (enumerate-interval 1 board-size)
;(cons (list 1 2) pair) for pair in ((1 1) (2 1) ... (8 1))
;(((1 2) (1 1)) ((1 2) (2 1)) ... ((1 2) (8 1)))
;---
;2.
;2 -> 2nd x of (enumerate-interval 1 board-size)
;(cons (list 2 2) pair) for pair in ((1 1) (2 1) ... (8 1))
;(((2 2) (1 1)) ((2 2) (2 1)) ... ((2 2) (8 1)))
;---
;...
;---
;8.
;8 -> 8th x of (enumerate-interval 1 board-size)
;(cons (list 8 2) pair) for pair in ((1 1) (2 1) ... (8 1))
;(((8 2) (1 1)) ((8 2) (2 1)) ... ((8 2) (8 1)))
;---
;final result for k=2 (after outer mapping)
;---
;((((1 2) (1 1)) ((1 2) (2 1)) ... ((1 2) (8 1)))
; (((2 2) (1 1)) ((2 2) (2 1)) ... ((2 2) (8 1)))
; ...
; (((8 2) (1 1)) ((8 2) (2 1)) ... ((8 2) (8 1))))
;---
;final result with flatmapping
;---
;(((1 2) (1 1)) ((1 2) (2 1)) ... ((1 2) (8 1))
; ((2 2) (1 1)) ((2 2) (2 1)) ... ((2 2) (8 1))
; ...
; ((8 2) (1 1)) ((8 2) (2 1)) ... ((8 2) (8 1)))
;---
;finally, filtering with 'safe?' (the filtering is performed by checking whether every
;newly added position is valid relative to all pre-existent combinations, that is, for
;every new position added to the board, it is checked whether it provokes any conflict
;with previous positions row-wise, column-wise and diagonal-wise)

;same reasoning applies for:
;---
;(queen-cols 3)
;(queen-cols 4)
;(queen-cols 5)
;(queen-cols 6)
;(queen-cols 7)
;(queen-cols 8)


;------------------------------------------------------------------------------------------
;PART 3
;(conclusions)
;------------------------------------------------------------------------------------------

;despite both versions of the program seeming to be very similar (only the order of the
;mappings performed is changed), we have to consider the fact that one of the sequences
;provided to 'map'/'flatmap' involves a recursive call to 'queen-cols'. in the first
;version of the program, the recursive call is made in the outer mapping, whereas in the
;second version the recursive call is performed in the inner mapping. this has a
;tremendous impact on the performance of the program, since, in the second version
;(contrary to what happens in the original one), for each call to 'queen-cols', the
;procedure is recursively called a total number of <board-size> times. this means that,
;if the first version of the program takes T units of time to process an input of size n
;(board-size), the second program will take a total of T^n units of time to finish its
;execution, which implies a huge loss in performance
;---
;nevertheless, loius' solution may be implemented in a much more efficient way. to avoid
;unnecessary recursive calls to 'queen-cols', the result of evaluating
;(queen-cols (- k 1)) might be stored and accessed by all instances of
;(enumerate-interval 1 board-size). the resulting program would be almost as efficient as
;the original version, only needing some extra time (maybe negligenciable) to store the
;result of calling (queen-cols (- k 1))
;---
;(flatmap
; (let ((prev-positions (queen-cols (- k 1))))
;   (lambda (new-row)
;     (my-map
;      (lambda (rest-of-queens)
;        (adjoin-positions new-row
;                          k
;                          rest-of-queens))
;      prev-positions)))
; (enumerate-interval 1 board-size))

