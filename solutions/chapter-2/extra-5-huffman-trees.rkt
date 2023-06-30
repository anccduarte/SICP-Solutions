
#lang sicp

;------------------------------------------------------------------------------------------
;HUFFMAN TREES
;------------------------------------------------------------------------------------------

;representation of leaves
;---
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
;---
(define (leaf? x)
  (eq? (car x) 'leaf))
;---
(define symbol-leaf cadr)
(define weight-leaf caddr)

;representation of huffman trees
;---
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
;---
(define left-branch car)
(define right-branch cadr)
;---
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
;---
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;adjoining element to an existing set
;---
(define (adjoin-set x set)
  (cond ((null? set)
         (cons x '()))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

;constructing a set of leaves (sorted by weight)
;---
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;generating a huffman tree from a list of pairs (symbol+weight)
;---
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;---
(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (let ((tree (make-code-tree (car set)
                                  (cadr set))))
        (successive-merge (adjoin-set tree (cddr set))))))

;decoding a message (bits -> symbols)
;---
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((new-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? new-branch)
              (cons (symbol-leaf new-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) new-branch)))))
  (decode-1 bits tree))
;---
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:" bit))))

;encoding a message (message -> bits)
;---
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;---
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
;---
(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((element-of-set? symbol (symbols left))
               (cons 0 (encode-symbol symbol left)))
              ((element-of-set? symbol (symbols right))
               (cons 1 (encode-symbol symbol right)))
              (else
               (error "bad symbol:" symbol))))))

;test
;---
(define huffman-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
;---
(define encoded-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display "encoded messsage -> ") encoded-message
;---
(define decoded-message (decode encoded-message huffman-tree))
(display "decoded message -> ") decoded-message
;---
(define re-encoded-message (encode decoded-message huffman-tree))
(display "re-encoded message -> ") re-encoded-message

