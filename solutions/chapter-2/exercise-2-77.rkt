
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.77.
;---
;Louis Reasoner tries to evaluate the expression '(magnitude z)' where 'z' is the object
;shown in Figure 2.24. (SEE BELOW *). To his surprise, instead of the answer '5' he gets
;an error message from 'apply-generic', saying there is no method for the operation
;'magnitude' on the types '(complex)'. He shows this interaction to Alyssa P. Hacker, who
;says "The problem is that the complex-number selectors were never defined for 'complex'
;numbers, just for polar and rectangular numbers. All you have to do to make this work is
;add the following to the complex package:
;---
;(put 'real-part '(complex) real-part)
;(put 'imag-part '(complex) imag-part)
;(put 'magnitude '(complex) magnitude)
;(put 'angle '(complex) angle)"
;---
;Describe in detail why this works. As an example, trace through all the procedures
;called in evaluating the expression '(magnitude z)' where 'z' is the object shown in
;Figure 2.24. (SEE BELOW *). In particular, how many times is 'apply-generic' invoked?
;What procedure is dispatched to in each case?
;------------------------------------------------------------------------------------------

;(*) visualizing the object 'z'
;---
;       +---+---+      +---+---+      +---+---+
; ----> | * | *------> | * | *------> | * | * |
;       +-|-+---+      +-|-+---+      +-|-+-|-+
;         |              |              |   |
;         v              v              v   v
;   +----------+   +--------------+    +-+ +-+
;   | 'complex |   | 'rectangular |    |3| |4|
;   +----------+   +--------------+    +-+ +-+

;the object 'z' is doubly tagged: the outer tag defines the type of number we are
;operating on and the inner tag defines the representation for complex numbers. the
;operation 'magnitude' is only defined for the data types 'rectangular' and 'polar', that
;is, types imposed at the inner level of tagging. calling 'magnitude' on 'z' produces a
;process that evolves somewhat like the following:
;---
;(magnitude z)
;(apply-generic 'magnitude z)
;((get 'magnitude (type z)) (contents z))
;((get 'magnitude 'complex) (contents z))
;(error "No method for these types: APPLY-GENERIC"
;       (list 'magnitude 'complex))
;No method for these types: APPLY-GENERIC (magnitude complex)

;installing 'magnitude' (and the remaining selectors for complex numbers) in the table
;solves this problem, since, in a first instance, 'apply-generic' strips off the 'complex
;tag and exposes the 'rectangular tag for a second application of the same procedure. the
;process evolves as follows:
;---
;(put 'magnitude '(complex) magnitude)
;(magnitude z)
;(apply-generic 'magnitude z)
;((get 'magnitude (type z)) (contents z))
;((get 'magnitude 'complex) '(rectangular 3 4))
;(magnitude '(rectangular 3 4))
;(apply-generic 'magnitude '(rectangular 3 4))
;((get 'magnitude (type '(rectangular 3 4))) (contents '(rectangular 3 4)))
;((get 'magnitude 'rectangular) '(3 4))
;(magnitude-rectangular 3 4)
;(sqrt (+ (square 3) (square 4)))
;5

;conclusions
;---
;'apply-generic' is invoked 2 times. first, the 'complex' tag is stripped off and the
;data is dispatched to the generic operator 'magnitude', which deals with both
;representations of complex numbers. in a second instance, the 'rectangular' tag is
;removed and the "real" contents of the data object are exposed. the exposed data is then 
;dispatched to the non-generic procedure 'magnitude-rectangular' that specifically
;selects the magnitude of complex numbers built under rectangular form

