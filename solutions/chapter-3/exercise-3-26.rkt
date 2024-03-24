
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.26.
;---
;To search a table as implemented above, one needs to scan through the list of records.
;This is basically the unordered list representation of Section 2.3.3. For large tables,
;it may be more efficient to structure the table in a different manner. Describe a table
;implementation where the (key, value) records are organized using a binary tree,
;assuming that keys can be ordered in some way (e.g., numerically or alphabetically).
;(Compare Exercise 2.66 of Chapter 2.)
;------------------------------------------------------------------------------------------


;pictoreal representation of tables [using trees]
;---
;note that the current representation does not need an indication '*table*' for the
;correct application of the operations 'lookup' and 'insert!'; moreover, given the
;singularities of comparison, the operation 'insert!' solely admits numbers as keys (in
;fact, it does not check for this condition, but using keys which are not numbers may
;raise an error in some cases)
;---
;(insert! '(1) 1 table)
;(insert! '(1 2) 2 table)
;(insert! '(2) 3 table)
;(insert! '(3) 4 table)
;(insert! '(3 4) 5 table)
;---
;            +---+---+---+     +---+---+---+     +---+---+---+
; table ---> | | | / | ------> | | | / | ------> | | | / | / |
;            +-|-+---+---+     +-|-+---+---+     +-|-+---+---+
;              ↓                 ↓                 ↓
;            +---+---+---+     +---+---+---+     +---+---+---+     +---+---+---+
;            | | | | | | |     | | | | | / |     | | | | | ------> | | | / | / |
;            +-|-+-|-+-|-+     +-|-+-|-+---+     +-|-+-|-+---+     +-|-+---+---+
;              ↓   ↓   |         ↓   ↓             ↓   ↓             ↓
;              1   1   |         2   3             3   4           +---+---+---+
;                      ↓                                           | | | | | / |
;                    +---+---+---+                                 +-|-+-|-+---+
;                    | | | | | / |                                   ↓   ↓
;                    +-|-+-|-+---+                                   4   5
;                      ↓   ↓
;                      2   2

;constructor, selectors and mutators for nodes
;---
(define (make-node key value subtree)
  (lambda (m) (m key
                 value
                 subtree
                 (lambda (x) (set! key x))
                 (lambda (x) (set! value x))
                 (lambda (x) (set! subtree x)))))
;---
(define (key-node node)
  (node (lambda (k v s sk sv ss) k)))
;---
(define (value-node node)
  (node (lambda (k v s sk sv ss) v)))
;---
(define (subtree-node node)
  (node (lambda (k v s sk sv ss) s)))
;---
(define (set-key-node! node x)
  (node (lambda (k v s sk sv ss) (sk x))))
;---
(define (set-value-node! node x)
  (node (lambda (k v s sk sv ss) (sv x))))
;---
(define (set-subtree-node! node x)
  (node (lambda (k v s sk sv ss) (ss x))))

;constructor, selectors and mutators for trees
;---
(define (make-tree node left right)
  (lambda (m) (m node
                 left
                 right
                 (lambda (x) (set! node x))
                 (lambda (x) (set! left x))
                 (lambda (x) (set! right x)))))
;---
(define (node-tree tree)
  (tree (lambda (n l r sn sl sr) n)))
;---
(define (left-tree tree)
  (tree (lambda (n l r sn sl sr) l)))
;---
(define (right-tree tree)
  (tree (lambda (n l r sn sl sr) r)))
;---
(define (set-node-tree! tree x)
  (tree (lambda (n l r sn sl sr) (sn x))))
;---
(define (set-left-tree! tree x)
  (tree (lambda (n l r sn sl sr) (sl x))))
;---
(define (set-right-tree! tree x)
  (tree (lambda (n l r sn sl sr) (sr x))))

;'empty-tree?' -> predicate verifying if empty tree
;---
(define (empty-tree? tree)
  (null? (key-node (node-tree tree))))

;'empty-tree' -> constructor for empty trees
;---
(define (empty-tree)
  (make-tree (make-node '() '() '())
             '()
             '()))

;'lookup'
;---
(define (lookup keys tree)
  (cond ((empty-tree? tree) #f)
        ((null? keys)
         (let ((value (value-node (node-tree tree))))
           (if (null? value) #f value)))
        (else
         (let ((key (key-node (node-tree tree))))
           (cond ((= (car keys) key)
                  (if (null? (cdr keys))
                      (lookup (cdr keys) tree)
                      (lookup (cdr keys) (subtree-node (node-tree tree)))))
                 ((< (car keys) key)
                  (lookup keys (left-tree tree)))
                 (else
                  (lookup keys (right-tree tree))))))))

;'insert!'
;---
(define (insert! keys value tree)
  ;---
  (define (set-empty-trees! tree)
    (set-subtree-node! (node-tree tree) (empty-tree))
    (set-left-tree! tree (empty-tree))
    (set-right-tree! tree (empty-tree)))
  ;---
  (define (branch-inwards! keys tree)
    (if (null? (cdr keys))
        (set-value-node! (node-tree tree) value)
        (insert! (cdr keys) value (subtree-node (node-tree tree)))))
  ;---
  (define (branch-outwards! keys tree selector set-tree!)
    (let ((new-tree (selector tree)))
      (if (null? new-tree) (set-tree! tree (empty-tree)))
      (if (empty-tree? new-tree)
          (begin (set-key-node! (node-tree new-tree) (car keys))
                 (set-empty-trees! new-tree)))
      (insert! keys value new-tree)))
  ;---
  (cond ((null? keys)
         (set-value-node! (node-tree tree) value))
        ((empty-tree? tree)
         (set-key-node! (node-tree tree) (car keys))
         (set-empty-trees! tree)
         (branch-inwards! keys tree))
        (else
         (cond ((= (car keys) (key-node (node-tree tree)))
                (branch-inwards! keys tree))
               ((< (car keys) (key-node (node-tree tree)))
                (branch-outwards! keys tree left-tree set-left-tree!))
               (else
                (branch-outwards! keys tree right-tree set-right-tree!))))))


;test table ['insert!' and 'lookup']
;---
(let ((tree (empty-tree))
      (keys '((1) (1 1) (1 2) (1 3) (1 4)
              (2) (2 1) (2 2) (2 3) (2 4)
              (3) (3 1) (3 2) (3 3) (3 4)
              (4) (4 1) (4 2) (4 3) (4 4)
              (5) (5 1) (5 2) (5 3) (5 4))))
  ;---
  (define (test-lookup keys)
    (display keys)
    (display " -> ")
    (display (lookup keys tree))
    (newline))
  ;---
  (insert! '(1) 1 tree)
  (insert! '(1 2) 2 tree)
  (insert! '(2) 3 tree)
  (insert! '(3) 4 tree)
  (insert! '(3 4) 5 tree)
  ;---
  (map test-lookup keys)
  'done)

