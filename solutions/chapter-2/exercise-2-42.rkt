
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.42.
;---
;The "eight-queens puzzle" asks how to place eight queens on a chessboard so that no
;queen is in check from any other (i.e., no two queens are in the same row, column, or
;diagonal). One possible solution is shown in Figure 2.8 (SEE BELOW *). One way to solve
;the puzzle is to work across the board, placing a queen in each column. Once we have
;placed k-1 queens, we must place the kth queen in a position where it does not check any
;of the queens already on the board. We can formulate this approach recursively: Assume
;that we have already generated the sequence of all possible ways to place k-1 queens in
;the first k-1 columns of the board. For each of these ways, generate an extended set of
;positions by placing a queen in each row of the kth column. Now filter these, keeping
;only the positions for which the queen in the kth column is safe with respect to the
;other queens. This produces the sequence of all ways to place k queens in the first k
;columns. By continuing this process, we will produce not only one solution, but all
;solutions to the puzzle. We implement this solution as a procedure 'queens', which
;returns a sequence of all solutions to the problem of placing n queens on an n x n
;chessboard. 'queens' has an internal procedure 'queen-cols' that returns the sequence of
;all ways to place queens in the first k columns of the board (SEE BELOW **). In this
;procedure, 'rest-of-queens' is a way to place k-1 queens in the first k-1 columns, and
;'new-row' is a proposed row in which to place the queen for the kth column. Complete the
;program by implementing the representation for sets of board positions, including the
;procedure 'adjoin-position', which adjoins a new row-column position to a set of
;positions, and 'empty-board', which represents an empty set of positions. You must also
;write the procedure 'safe?', which determines for a set of positions, whether the queen
;in the kth column is safe with respect to the others. (Note that we need only check
;whether the new queen is safe - the other queens are already guaranteed safe with
;respect to each other.)
;------------------------------------------------------------------------------------------

;(*) example -> a solution to the eight-queens puzzle
;---
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;|   |   |   |   |   | Q |   |   |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;|   |   | Q |   |   |   |   |   |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;| Q |   |   |   |   |   |   |   |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;|   |   |   |   |   |   | Q |   |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;|   |   |   |   | Q |   |   |   |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;|   |   |   |   |   |   |   | Q |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;|   | Q |   |   |   |   |   |   |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;|   |   |   | Q |   |   |   |   |
;+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

;helper procedures -> operations on lists
;---
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))
;---
(define (my-map proc lst)
  (if (null? lst)
      nil
      (cons (proc (car lst))
            (my-map proc (cdr lst)))))
;---
(define (filter predicate? lst)
  (cond ((null? lst) nil)
        ((not (predicate? (car lst)))
         (filter predicate? (cdr lst)))
        (else
         (cons (car lst)
               (filter predicate? (cdr lst))))))
;---
(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (accumulate op init (cdr lst)))))
;---
(define (flatmap proc seq)
  (accumulate append nil (my-map proc seq)))
;---
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))

;'empty-board'
;---
(define empty-board nil)

;'adjoin-position'
;---
(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

;'safe?'
;['positions' is a list of lists, each one of the latter lists representing a coordinate
;(x y) of a queen. the goal is to check whether the first queen (the last to be added)
;lives in a row, column or diagonal already occupied by a previously added queen]
;---
(define (safe-row? curr seq)
  (let ((x (car curr))
        (other-xs (my-map (lambda (seq-pair) (car seq-pair)) seq)))
    (null? (filter (lambda (x-cor) (= x-cor x))
                   other-xs))))
;---
(define (safe-column? curr seq)
  (let ((y (cadr curr))
        (other-ys (my-map (lambda (seq-pair) (cadr seq-pair)) seq)))
    (null? (filter (lambda (y-cor) (= y-cor y))
                   other-ys))))
;---
(define (safe-diagonal? curr seq)
  (let ((x (car curr)) (y (cadr curr)))
    (null? (filter (lambda (seq-pair)
                     (let ((x-cor (car seq-pair))
                           (y-cor (cadr seq-pair)))
                       (= (abs (- x x-cor))
                          (abs (- y y-cor)))))
                   seq))))
;---
(define (safe? positions)
  (let ((curr-position (car positions))
        (rest-positions (cdr positions)))
    (and
     (safe-row? curr-position rest-positions)
     (safe-column? curr-position rest-positions)
     (safe-diagonal? curr-position rest-positions))))

;(**) 'queens'
;(the procedure 'safe?' does not use the argument 'k'; the most recently added queen is
;always placed in the column 0 of the data structure; hence, there is no need to wrap it
;in a lambda function)
;---
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         safe?
         (flatmap
          (lambda (rest-of-queens)
            (my-map (lambda (new-row)
                      (adjoin-position
                       new-row k rest-of-queens))
                    (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;test for random board-sizes
;---
(define (for-each proc lst)
  (cond ((null? lst)
         (display ""))
        (else
         (proc (car lst))
         (for-each proc (cdr lst)))))
;---
(define (test k)
  (display "board size = ") (display k) (newline)
  (display "---") (newline)
  (for-each (lambda (x) (display x) (newline)) (queens k))
  (newline))
;---
(test 4)
(test 5)

