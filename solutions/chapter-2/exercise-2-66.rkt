
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.66.
;---
;Implement the 'lookup' procedure for the case where the set of records is structured as
;a binary tree, ordered by the numerical values of the keys.
;------------------------------------------------------------------------------------------

;"We have examined options for using lists to represent sets and have seen how the choice
;of representation for a data object can have a large impact on the performance of the
;programs that use the data. Another reason for concentrating on sets is that the
;techniques discussed here appear again and again in applications involving information
;retrieval [e.g., accessing or modifying data records in a data bases]."

;'key' -> selector for a record's key
;(it is assumed that the key of each record corresponds to its first element)
;---
(define key car)

;selectors for binary trees
;---
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

;'lookup-unord'
;(search for a record in a set 'set-of-records' having an identifying key 'given-key'; it
;is assumed that the input set is represented as an unordered list)
;---
(define (lookup-unord given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else
         (lookup-unord given-key (cdr set-of-records)))))

;"Of course, there are better ways to represent large sets than as unordered lists.
;Information-retrieval systems in which records have to be 'randomly accessed' are
;typically implemented by a tree-based method, such as the binary-tree representation
;discussed previously. In designing such a system the methodology of data abstraction can
;be a great help. The designer can create an initial implementation using a simple,
;straightforward representation such as unordered lists. This will be unsuitable for the
;eventual system, but it can be useful in providing a 'quick and dirty' data base with
;which to test the rest of the system. Later on, the data representation can be modified
;to be more sophisticated. If the data base is accessed in terms of abstract selectors
;and constructors, this change in representation will not require any changes to the rest
;of the system."

;'lookup-bin'
;(same as before, but the set of records is represented as a binary tree; also, it is
;assumed that the values of the keys are numeric)
;---
(define (lookup-bin given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup-bin given-key
                     (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup-bin given-key
                     (right-branch set-of-records)))))

;test for random set of records
;---
(define set-of-records '((3 "James" 21)
                         ((1 "Robert" 30)
                          ()
                          ((2 "John" 18) () ()))
                         ((5 "Mary" 22)
                          ((4 "Patricia" 31) () ())
                          ((6 "Jennifer" 19) () ()))))
;---
(for-each (lambda (x)
            (display "key=") (display x) (display " -> ")
            (display (lookup-bin x set-of-records))
            (newline))
          '(0 1 2 3 4 5 6 7))

