
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.74.
;---
;Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting
;of a large number of independent divisions located all over the world. The company's
;computer facilities have just been interconnected by means of a clever
;network-interfacing scheme that makes the entire network appear to any user to be a
;single computer. Insatiable's president, in her first attempt to exploit the ability of
;the network to extract administrative information from division files, is dismayed to
;discover that, although all the division files have been implemented as data structures
;in Scheme, the particular data structure used varies from division to division. A
;meeting of division managers is hastily called to search for a strategy to integrate the
;files that will satisfy headquarters' needs while preserving the existing autonomy of
;the divisions. Show how such a strategy can be implemented with data-directed
;programming. As an example, suppose that each division's personnel records consist of a
;single file, which contains a set of records keyed on employees' names. The structure of
;the set varies from division to division. Furthermore, each employee's record is itself
;a set (structured differently from division to division) that contains information keyed
;under identifiers such as address and salary. In particular:
;---
;(a) Implement for headquarters a "get-record" procedure that retrieves a specified
;employee's record from a specified personnel file. The procedure should be applicable to
;any division's file. Explain how the individual divisions' files should be structured.
;In particular, what type information must be supplied?
;---
;(b) Implement for headquarters a "get-salary" procedure that returns the salary
;information from a given employee's record from any division's personnel file. How
;should the record be structured in order to make this operation work?
;---
;(c) Implement for headquarters a "find-employee-record" procedure. This should search
;all the divisions' files for the record of a given employee and return the record.
;Assume that this procedure takes as arguments an employee's name and a list of all the
;divisions' files.
;---
;(d) When Insatiable takes over a new company, what changes must be made in order to
;incorporate the new personnel information into the central system?
;------------------------------------------------------------------------------------------

;(a)
;---
;Any division's file should consist of a pair composed of a tag denoting the name of the
;division and the contents of the file themselves. The contents of the file are a set of
;sets, each set representing an emplyee's record. Each set is origanized differently
;depending on the particular division and contains information like the name (the key
;used to retrieve a record from a division's file), the address and the salary of the
;employee. Each division must have selectors for retrieving this information. The table
;used by a generic operator "get-record" would look somewhat like the following (note
;that, as stated in the question, each procedure sitting in the table accepts two
;arguments, namely the name of an employee and the division's personnel file)
;---
;              |      'div1      |      'div2      |      'div3      | ...
; -------------+-----------------+-----------------+-----------------+-----    
;  'get-record | get-record-div1 | get-record-div2 | get-record-div3 | ...
;---
;To tag each division's personnel file and to retrieve both the tag and the contents of
;the file, the following procedures would be useful
;---
(define (tag-file division file) (cons division file))
(define (division tagged-file) (car tagged-file))
(define (records tagged-file) (cdr tagged-file))
;---
;A generic procedure "get-record" may now be constructed by dispatching the data it
;receives to the right procedure in the table (strip off the tag 'tag and select the
;procedure stored under the combination ('get-record, 'tag))
;---
(define (get-record name file)
  (let ((proc (get 'get-record (division file))))
    (proc name (records file))))
;---
;Each division's procedure for finding a record based on the name of the employee would,
;for example, be implemented and installed in the system as
;---
(define (install-get-record-div1)
  ;defining a selector for the name of an employee in a file
  ;(the decision on the structure of each personnel record is
  ;dependent on the particular division; in this case, the name
  ;of the employee is the first element of the record)
  (define (get-name record) (car record))
  ;defining "get-record"
  (define (get-record name file)
    (if (null? file)
        #f
        (let ((record (car file)))
          (if (eq? (get-name record) name)
              record
              (get-record name (cdr file))))))
  ;installing the procedure in the system
  (put 'get-record 'div1 get-record))

;(b)
;---
;Each employee's record must obviously have information on the employee's salary for the
;procedure to work. However, it doesn't matter which record structure each particular
;division follows. Provided that each division implements a selector for retrieving
;salary information, "get-salary" is guaranteed to work fine. Now, the table looks like
;---
;              |      'div1      |      'div2      |      'div3      | ...
; -------------+-----------------+-----------------+-----------------+-----    
;  'get-record | get-record-div1 | get-record-div2 | get-record-div3 | ...
; -------------+-----------------+-----------------+-----------------+-----
;  'get-salary | get-salary-div1 | get-salary-div2 | get-salary-div3 | ...
;---
;"get-salary" implementation is quite similar to "get-record"'s. They only differ in the
;first index used by the dispatcher
;---
(define (get-salary name file)
  (let ((proc (get 'get-salary (division file))))
    (proc name (records file))))
;---
;"get-salary" would make use of the generic procedure "get-record" and then apply the
;appropriate selector for the salary depending on the division (note that all division's
;procedures may be installed simultaneously, that is, the selectors for the records, and
;salary and adress of an employee might have been installed via a single procedure; the
;provided implemenation is just easier to follow given the structure of the question)
;---
(define (install-get-salary-div1)
  ;defining "get-salary"
  ;(assume that the salary of an employee of this particular
  ;division is the second entry of the respective record)
  (define (get-salary name file)
    (let ((record (get 'get-record 'div1)))
      (if record
          (cadr record)
          #f)))
  ;installing the procedure in the system
  (put 'get-salary 'div1 get-salary))

;(c)
;---
;The implementation of "find-employee-record" is quite straightforward. Given that all
;records are tagged with the division name, one just need to apply the generic procedure
;"get-record" for each of the files "find-employee-record" takes as input (note that
;the procedure "get-record" returns #f whenever there is no record in the file whose
;associated name matches the name provided as an argument, hence, the next division's
;file must be inspected; if "get-record" does yield a record, simply return it)
;---
(define (find-employee-record name personnel-files)
  (if (null? personnel-files)
      #f
      (let ((file (car personnel-files)))
        (let ((record (get-record name file)))
          (if record
              record
              (find-employee-record name (cdr personnel-files)))))))

;(d)
;---
;The beauty of a data-directed approach lies in the fact that the addition of new data
;representations to a particular system is done quite effortlessly. Provided that the
;new company already has the selectors implemented for their own use, Insatiable does not
;need to take any action in order to incorporate the new personnel information into the
;system (other than adding the new personnel file to the existing list of personnel
;files). The responsibility is entirely on the new company's hands, and the execution of
;their task couldn't be simpler. They must simply install their already implemented
;procedures into the table and label its personnel file with an adequate tag!

