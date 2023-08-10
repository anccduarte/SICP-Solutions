
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.76.
;---
;As a large system with generic operations evolves, new types of data objects or new
;operations may be needed. For each of the three strategies - generic operations with
;explicit dispatch, data-directed style, and message-passing style - describe the changes
;that must be made to a system in order to add new types or new operations. Which
;organization would be most appropriate for a system in which new types must often be
;added? Which would be most appropriate for a system in which new operations must often
;be added?
;------------------------------------------------------------------------------------------

;---

;EXPLICIT DISPATCH
;(i.e., generic procedures that dispatch data to the appropriate non-generic procedure
;depending on the type of the data)
;---
;every time an OPERATION is added, a new generic procedure for that operation must be
;implemented; this involves creating new non-generic procedures for each existing data
;type, and building a generic operator that dispatches the data to the right procedure
;according to its type
;---
;every time a DATA TYPE is added, new specific procedures for that type must first be
;implemented (assuming they are not already implemented); then, each specific procedure
;must be installed in the system by updating the respective generic operator (a new test
;case is added to each generic operator)

;DATA-DIRECTED STYLE
;(i.e., table storing (operation, data type) combinations)
;---
;every time an OPERATION is added, a new row representing that operation must be added to
;the table; this involves creating new procedures performing the operation for each
;specific data type, and appropriately adding them to the table
;---
;every time a DATA TYPE is added, a new column representing that data type must be added
;to the table; this involves implementing new non-generic procedures for the new data
;type performing each operation defined in the table, and adding them accordingly

;MESSAGE-PASSING STYLE
;(i.e., the allowed operations on a data type are defined upon construction of an object
;of that specific type - the object returns a procedure that dispatches the operation it
;takes as input)
;---
;every time an OPERATION is added, for every data representation, a new case (evaluation
;for the operation and action to perform if the test is passed) must be added to the case
;analysis embedded in the procedure returned for that data type
;---
;every time a DATA TYPE is added, a new procedure for that data representation must be
;created; this procedure must return an operation dispatcher embedding a case analysis
;consisting of tests on operations implemented for the remaining data representations on
;the system

;---

;Which organization would be most appropriate for a system in which new TYPES must often
;be added?
;---
;the task of adding a new data type is pretty straightforward in both data-directed and
;message-passing styles. the procedures concerning that data type are implemented
;independent on the procedures respective to the remaining data types and the generic
;procedures, defined in terms of "apply-generic", remain untouched. the extra step
;of installing the procedures in a table slightly disfavors the data-directed style,
;making the message-passing style more appropriate whenever new data types are often
;added to the system

;---

;Which would be most appropriate for a system in which new OPERATIONS must often be
;added?
;---
;adding new operations to existing data types might be troublesome if a strategy
;involving either explicit dispatch or message-passing is employed. this is the case
;since whenever an operation is added the existing procedures (generic operators or
;constructors) must be modified to accomodate the new operation (the case analysis
;inherent to these strategies must be extended in both cases). data-directed style does
;not suffer from such constraints as both the procedure selector ("get") and the generic
;operators (implemented in terms of "apply-generic") remain unchanged: the new procedures
;representing new operation(s) must simply be added to the existing table. this makes
;data-directed style a more appropriate strategy whenever new operations are frequently
;added to the system

