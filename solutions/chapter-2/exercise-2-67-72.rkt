
#lang sicp


;------------------------------------------------------------------------------------------
;EXAMPLE: HUFFMAN ENCODING TREES (2.67. to 2.72.)
;------------------------------------------------------------------------------------------

;leaf representation
;---
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
;---
(define (leaf? x)
  (eq? (car x) 'leaf))
;---
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;huffman tree representation
;---
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
;---
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
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

;adjoining a leaf to an ordered set (the order of the elements in the set is determined
;by the weights of the leafs composing the set)
;---
(define (adjoin-set x set)
  (cond ((null? set)
         (cons x '()))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

;making an ordered set of leaf nodes from a list of pairs (each pair containing a symbol
;as the first element and a frequency/weight as the second element)
;---
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs))) ;current pair
        (adjoin-set (make-leaf (car pair) ;symbol of 'pair'
                               (cadr pair)) ;weight of 'pair'
                    (make-leaf-set (cdr pairs)))))) ;remaining pairs

;decoding a message (bits -> symbols)
;---
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
;---
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


;------------------------------------------------------------------------------------------
;EXERCISE 2.67.
;---
;Define an encoding tree and a sample message (SEE BELOW *). Use the 'decode' procedure
;to decode the message, and give the result.
;------------------------------------------------------------------------------------------

;(*) defining an encoding tree and a sample message
;---
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
;---
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;decoding the message
;---
(display "EXERCISE 2.67.") (newline)
;---
(define decoded-message-2-67 (decode sample-message sample-tree))
(display sample-message) (display " -> ") (display decoded-message-2-67) (newline)


;------------------------------------------------------------------------------------------
;EXERCISE 2.68.
;---
;The 'encode' procedure takes as arguments a message and a tree and produces the list of
;bits that gives the encoded message (SEE BELOW **). 'encode-symbol' is a procedure,
;which you must write, that returns the list of bits that encodes a given symbol
;according to a given tree. You should design 'encode-symbol' so that it signals an error
;if the symbol is not in the tree at all. Test your procedure by encoding the result you
;obtained in Exercise 2.67 with the sample tree and seeing whether it is the same as the
;original sample message.
;------------------------------------------------------------------------------------------

;(**) encoding a message (symbols -> bits)
;---
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;'element-of-set?' -> helper
;(the whole set of symbols must be inspected since there is no guarantee that the symbols
;are in lexicographical order)
;---
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;'encode-symbol' transforms a symbol into a sequence of bits by traversing a huffman
;encoding tree (if we can reach a leaf node, the symbol is in the tree and the sequence
;of bits is returned; otherwise, an error message is raised)
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

;re-encoding the previously decoded message
;---
(display "---") (newline) (display "EXERCISE 2.68.") (newline)
;---
(define re-encoded-message (encode decoded-message-2-67 sample-tree))
(display decoded-message-2-67) (display " -> ") (display re-encoded-message) (newline)

;rigorously testing the encoded and re-encoded message for equality
;---
(define (equal? s1 s2)
  (or (eq? s1 s2)
      (and (pair? s1)
           (pair? s2)
           (equal? (car s1) (car s2))
           (equal? (cdr s1) (cdr s2)))))
;---
(display sample-message) (display " =? ") (display re-encoded-message)
(display " -> ") (display (equal? sample-message re-encoded-message)) (newline)


;------------------------------------------------------------------------------------------
;EXERCISE 2.69.
;---
;The following procedure takes as its argument a list of symbol-frequency pairs (where no
;symbol appears in more than one pair) and generates a Huffman encoding tree according to
;the Huffman algorithm (SEE BELOW ***). 'make-leaf-set' is the procedure given above that
;transforms the list of pairs into an ordered set of leaves. 'successive-merge' is the
;procedure you must write, using 'make-code-tree' to successively merge the
;smallest-weight elements of the set until there is only one element left, which is the
;desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If
;you find yourself designing a complex procedure, then you are almost certainly doing
;something wrong. You can take significant advantage of the fact that we are using an
;ordered set representation.)
;------------------------------------------------------------------------------------------

;(***) generating a huffman tree from a set of pairs
;(note that the input pairs are transformed and sorted by 'make-leaf-set' before being
;processed by the procedure 'successive-merge' wrapped by 'generate-huffman-tree')
;---
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;'successive-merge'
;(at every call, we want to: 1. check whether the set only has one element [if it is the
;case, simply return the 'car' of the set]; 2. select the trees/leafes with the lowest
;weights, i.e., the 'car' and the 'cadr' of the set; 3. merge the previous trees/leaves;
;4. construct a new sorted set, using 'adjoin-set', composed of the newly merged entities
;and the remaining constituents of the previous set ('cddr'); 5. call 'successive-merge'
;on the newly created set)
;---
(define (successive-merge leaf-set)
  ;step 1 [stop if true]
  (if (null? (cdr leaf-set))
      (car leaf-set)
      ;steps 2 and 3
      (let ((tree (make-code-tree (car leaf-set)
                                  (cadr leaf-set))))
        ;steps 4 and 5
        (successive-merge (adjoin-set tree (cddr leaf-set))))))

;test for 'sample-message'
;(the generated tree is equivalent to the manually generated 'sample-tree')
;---
(display "---") (newline) (display "EXERCISE 2.69.") (newline)
;---
(define pairs-2-69 '((A 4) (B 2) (C 1) (D 1)))
(define tree-2-69 (generate-huffman-tree pairs-2-69))
;---
(define decoded-message-2-69 (decode sample-message tree-2-69))
(display sample-message) (display " -> ") (display decoded-message-2-69) (newline)


;------------------------------------------------------------------------------------------
;EXERCISE 2.70.
;---
;The following eight-symbol alphabet with associated relative frequencies was designed to
;efficiently encode the lyrics of 1950s rock songs. (Note that the "symbols" of an
;"alphabet" need not be individual letters.)
;---
;A    2   GET 2   SHA 3   WAH 1
;BOOM 1   JOB 2   NA 16   YIP 9
;---
;Use 'generate-huffman-tree' (Exercise 2.69) to generate a corresponding Huffman tree,
;and use 'encode' (Exercise 2.68) to encode the following message:
;---
;Get a job
;Sha na na na na na na na na
;Get a job
;Sha na na na na na na na na
;Wah yip yip yip yip yip yip yip yip yip
;Sha boom
;---
;How many bits are required for the encoding? What is the smallest number of bits that
;would be needed to encode this song if we used a fixed-length code for the eight-symbol
;alphabet?
;------------------------------------------------------------------------------------------

;defining the pairs (symbol+weight)
;---
(define pairs-2-70 '((A 2) (BOOM 1) (GET 2) (JOB 2) (SHA 3) (NA 16) (WAH 1) (YIP 9)))

;constructing the message to be encoded
;---
(define message-2-70 '(GET A JOB
                       SHA NA NA NA NA NA NA NA NA
                       GET A JOB
                       SHA NA NA NA NA NA NA NA NA
                       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                       SHA BOOM))

;constructing the huffman tree
;---
(define tree-2-70 (generate-huffman-tree pairs-2-70))

;encoding the message
;---
(display "---") (newline) (display "EXERCISE 2.70.") (newline)
;---
(define encoded-message-2-70 (encode message-2-70 tree-2-70))
(display "message -> ") (display encoded-message-2-70) (newline)
(display "length -> ") (display (length encoded-message-2-70)) (newline)

;conclusions
;---
;to encode the message using a variable-length code generated by a huffman tree we need a
;total of 84 bits. if we were to use a fixed-length code, since we have 8 distinct
;symbols, we would have to use log2(8) = 3 bits to represent each symbol in the message.
;considering that the message is composed of 36 symbols, the total number of bits needed
;to encode the message using a fixed-length code would be 108. thus, by using a
;variable-length code we were able to save more than 20% in space in comparison with the
;fixed-length code


;------------------------------------------------------------------------------------------
;EXERCISE 2.71.
;---
;Suppose we have a Huffman tree for an alphabet of 'n' symbols, and that the relative
;frequencies of the symbols are 1, 2, 4, ..., 2^(n-1). Sketch the tree for n=5; for n=10.
;In such a tree (for general 'n') how many bits are required to encode the most frequent
;symbol? The least frequent symbol?
;------------------------------------------------------------------------------------------

;merging for n=5
;---
;initial leaves -> {(N1 1) (N2 2) (N3 4) (N4 8) (N5 16)}
;1st merge      -> {({N1 N2} 3) (N3 4) (N4 8) (N5 16)}
;2nd merge      -> {({N1 N2 N3} 7) (N4 8) (N5 16)}
;3rd merge      -> {({N1 N2 N3 N4} 15) (N5 16)}
;4th merge      -> {({N1 N2 N3 N4 N5} 31)}

;huffman tree for n=5
;---
;                   {N1 N2 N3 N4 N5} 31
;                   /              \
;            {N1 N2 N3 N4} 15      N5 16
;             /           \
;      {N1 N2 N3} 7      N4 8
;      /        \
;   {N1 N2} 3   N3 4
;   /     \
; N1 1    N2 2

;merging for n=10
;---
;initial leaves -> {(A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;1st merge      -> {({A B} 3) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;2nd merge      -> {({A B C} 7) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;3rd merge      -> {({A B C D} 15) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;4th merge      -> {({A B C D E} 31) (F 32) (G 64) (H 128) (I 256) (J 512)}
;5th merge      -> {({A B C D E F} 63) (G 64) (H 128) (I 256) (J 512)}
;6th merge      -> {({A B C D E F G} 127) (H 128) (I 256) (J 512)}
;7th merge      -> {({A B C D E F G H} 255) (I 256) (J 512)}
;8th merge      -> {({A B C D E F G H} 511) (J 512)}
;9th merge      -> {({A B C D E F G H J} 1023)}

;huffman tree for n=10
;---
;                                                       {A B C D E F G H I J} 1023
;                                                       /                   \
;                                             {A B C D E F G H I} 511        J 512
;                                             /                 \
;                                    {A B C D E F G H} 255       I 256
;                                    /               \
;                            {A B C D E F G} 127      H 128
;                            /             \
;                     {A B C D E F} 63      G 64
;                     /           \
;               {A B C D E} 31     F 32
;               /         \
;         {A B C D} 15     E 16
;         /       \
;     {A B C} 7    D 8
;     /     \
;  {A B} 3   C 4
;  /   \
; A 1   B 2

;conclusions
;---
;given these relative frequencies, the resulting huffman tree will be highly unbalanced.
;this is the case, since, for all n-2 intermediate merges, the product of merging the
;trees having the smallest weights always produces a tree with a weight smaller than the
;weight of the subsequent leaf (the leaf having the third smallest weight). this happens
;for this particular set of frequencies/weights because, for all integer values of 'n',
;the sum of 2^(n-2) and 2^(n-1) always equals (2^n)-1. hence, at every level, the left
;branch of the tree contains a set of the so far merged symbols and the right branch of
;the tree simply stores the next symbol to be added to the parent set
;---
;concerning the particular cases presented above:
;n=5  -> 1 bit for most frequent symbol; 4 bits for least frequent symbol
;n=10 -> 1 bit for most frequent symbol; 9 bits for least frequent symbol
;---
;for a general value of 'n'
;1 bit for the most frequent symbol; n-1 bits for the least frequent symbol


;------------------------------------------------------------------------------------------
;EXERCISE 2.72.
;---
;Consider the encoding procedure that you designed in Exercise 2.68. What is the order of
;growth in the number of steps needed to encode a symbol? Be sure to include the number
;of steps needed to search the symbol list at each node encountered. To answer this
;question in general is difficult. Consider the special case where the relative
;frequencies of the 'n' symbols are as described in Exercise 2.71, and give the order of
;growth (as a function of 'n') of the number of steps needed to encode the most frequent
;and least frequent symbols in the alphabet.
;------------------------------------------------------------------------------------------

;let's consider once again the previous example in which the set of initial leaves is
;given by T = {(N[1] 1) (N[2] 2) (N[3] 4) ... (N[n-1] 2^(n-2)) (N[n] 2^(n-1))}. from the
;above answer, it may be inferred that this constitutes the worst case scenario when
;constructing a huffman tree, that is, there is no way of attributing weights to symbols
;such that the resulting tree is more unbalanced than 'T'. this is the case, because, as
;previously stated, when merging two trees, the weight of the next leaf (the one with the
;third lowest weight) always surpasses the total weight of the newly merged tree by one
;unit. this means that, at every level of the huffman tree, if the corresponding node is
;characterized by a set 'S', the left branch at that level is the set S-{x}, and the
;right branch is simply 'x' (with 'x' being a symbol). hence, in the best case scenario,
;to decode a symbol, only one step is needed. however, in the worst case scenario, n-1
;steps need to be taken. [in the average case, the number of steps will then be
;approximately n/2.] moreover, at each level of the tree an 'element-of-set?' check needs
;to be performed. given the arrangement of the initial set of leaves, the total amount of
;steps needeed to be taken for checking each symbol along the tree has an order of growth
;of O(n). thus, multypling the latter orders of growth yields an overall order of growth
;of O(n^2) for the encoding of a single symbol. additionally, considering a message of
;size 'm', 'm' symbols have to be processed. obviously, adding one symbol to the message
;increases the number of symbols to be encoded by one, that is, we have a process that
;grows linearly with the size of the message. thus, its order of growth is O(m). finally,
;by multiplying the orders of growth, we get a total order of growth of O(m*n^2)

