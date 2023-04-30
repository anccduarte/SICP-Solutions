
#lang sicp


;------------------------------------------------------------------------------------------
;EXAMPLE: TESTING FOR PRIMALITY (1.21. to 1.28.)
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;EXERCISE 1.21.
;---
;Use the 'smallest-divisor' (SEE BELOW *) procedure to find the smallest divisor of each
;of the following numbers: 199, 1999, 19999.
;------------------------------------------------------------------------------------------

;helper procedures
;---
(define square (lambda (x) (* x x)))
;---
(define divides? (lambda (a b) (= (remainder a b) 0)))
;---
;"The end test for 'find-divisor' is based on the fact that if n is not prime it must
;have a divisor less than or equal to sqrt(n). (If d is a divisor of n, then so is n/d.
;But d and n/d cannot both be greater than sqrt(n).)"
(define (find-divisor n test-div)
  (cond ((> (square test-div) n) n)
        ((divides? n test-div) test-div)
        (else (find-divisor n (inc test-div)))))

;(*) 'smallest-divisor' procedure
;---
(define (smallest-divisor n) (find-divisor n 2))

;test smallest divisor for the numbers 199, 1999 and 19999
;---
(display "EXERCISE 1.21.") (newline)
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)


;------------------------------------------------------------------------------------------
;EXERCISE 1.22.
;---
;Most Lisp implementations include a primitive called 'runtime' that returns an integer
;that specifies the amount of time the system has been running (measured, for example, in
;microseconds). The following 'timed-prime-test' procedure (SEE BELOW **), when called
;with an integer n, prints n and checks to see if n is prime. If n is prime, the
;procedure prints three asterisks followed by the amount of time used in performing the
;test. Using this procedure, write a procedure 'search-for-primes' that checks the
;primality of consecutive odd integers in a specified range. Use your procedure to find
;the three smallest primes larger than 1000; larger than 10,000; larger than 100,000;
;larger than 1,000,000. Note the time needed to test each prime. Since the testing
;algorithm has order of growth of O(sqrt(n)), you should expect that testing for primes
;around 10,000 should take about sqrt(10) times as long as testing for primes around
;1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000
;support the O(sqrt(n)) prediction? Is your result compatible with the notion that
;programs on your machine run in time proportional to the number of steps required for
;the computation?
;------------------------------------------------------------------------------------------

;procedure that checks the primality of a given number n
;---
(define prime? (lambda (n) (= (smallest-divisor n) n)))

;helper procedures
;---
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
;---
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;(**) 'timed-prime-test' procedure
;---
(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime))
  (newline))

;ANSWER TO EXERCISE -> 'search-for-primes' procedure
;---
(define even? (lambda (n) (= (remainder n 2) 0)))
;---
(define (search-for-primes a b)
  (define (prime-iter x y)
    (cond ((> x y)
           (display ""))
          (else
           (timed-prime-test x)
           (prime-iter (+ x 2) y))))
  (cond ((< (* a b) 0)
         (error "The interval [a,b] contains negative numbers!"))
        ((> a b)
         (search-for-primes b a))
        ((even? a)
         (search-for-primes (inc a) b))
        (else
         (prime-iter a b))))

;test 'search-for-primes' for numbers 1000, 10000, 100000 and 1000000 
;---
(newline) (display "EXERCISE 1.22.") (newline)
;---
(display "- primes larger than 1000")
(newline) (search-for-primes 1000 1050)
;---
(display "- primes larger than 10000")
(newline) (search-for-primes 10000 10050)
;---
(display "- primes larger than 100000")
(newline) (search-for-primes 100000 100050)
;---
(display "- primes larger than 1000000")
(newline) (search-for-primes 1000000 1000050)

;results
;---
;3 primes larger than 1000
;1009 *** 2 | 1013 *** 2 | 1019 *** 2 -> average = 2 microseconds
;---
;3 primes larger than 10000
;10007 *** 4 | 10009 *** 5 | 10037 *** 5 -> average = 4.66 microseconds
;---
;3 primes larger than 100000
;100003 *** 13 | 100019 *** 14 | 100043 *** 14 -> average = 13.66 microseconds
;---
;3 primes larger than 1000000
;1000003 *** 41 | 1000033 *** 40 | 1000037 *** 40 -> average = 40.33 microseconds

;do the results support the O(sqrt(n)) prediction?
;---
(display "- checking the O(sqrt(n)) prediction") (newline)
;---
;1000 to 10000: 2*sqrt(10)~=4.66?
(* 2 (sqrt 10)) ;6.32 (a bit larger than 4.66)
;---
;10000 to 100000: 4.66*sqrt(10)~=13.66?
(* 4.66 (sqrt 10)) ;14.74 (a bit larger than 13.66)
;---
;100000 to 1000000: 13.66*sqrt(10)~=40.33?
(* 13.66 (sqrt 10)) ;43.20 (a bit larger than 40.33)

;conclusion
;---
;overall, the prediction that 'prime?' has order of growth O(sqrt(n)) holds true.
;strangely, the attained runtimes are slighly smaller than the actual predictions (i.e.,
;average(runtimes(i)) < average(runtimes(i-1)) * sqrt(10), 1 < i < 4)


;------------------------------------------------------------------------------------------
;EXERCISE 1.23.
;---
;The 'smallest-divisor' procedure shown at the start of this section does lots of
;needless testing: After it checks to see if the number is divisible by 2 there is no
;point in checking to see if it is divisible by any larger even numbers. This suggests
;that the values used for 'test-divisor' should not be 2, 3, 4, 5, 6, ..., but rather
;2, 3, 5, 7, 9, .... To implement this change, define a procedure 'next' that returns 3
;if its input is equal to 2 and otherwise returns its input plus 2. Modify the
;'smallest-divisor' procedure to use (next test-divisor) instead of (+ test-divisor 1).
;With 'timed-prime-test' incorporating this modified version of smallest-divisor, run the
;test for each of the 12 primes found in Exercise 1.22. Since this modification halves
;the number of test steps, you should expect it to run about twice as fast. Is this
;expectation confirmed? If not, what is the observed ratio of the speeds of the two
;algorithms, and how do you explain the fact that it is different from 2?
;------------------------------------------------------------------------------------------

;'next' procedure
;---
(define (next x) (if (= x 2) 3 (+ x 2)))

;redifining 'find-divisor' -> 'find-divisor-v2'
;---
(define (find-divisor-v2 n test-div)
  (cond ((> (square test-div) n) n)
        ((divides? n test-div) test-div)
        (else (find-divisor-v2 n (next test-div)))))

;redifining 'smallest-divisor' -> 'smallest-divisor-v2'
;---
(define (smallest-divisor-v2 n) (find-divisor-v2 n 2))

;redifining 'prime?' -> 'prime?-v2'
;---
(define prime?-v2 (lambda (n) (= (smallest-divisor-v2 n) n)))

;redifining 'start-prime-test' -> 'start-prime-test-v2'
;---
(define (start-prime-test-v2 n start-time)
  (if (prime?-v2 n)
      (report-prime (- (runtime) start-time))))

;redifining 'timed-prime-test' -> 'timed-prime-test-v2'
;---
(define (timed-prime-test-v2 n)
  (display n)
  (start-prime-test-v2 n (runtime))
  (newline))

;redifining 'search-for-primes' -> 'search-for-primes-v2'
;---
(define (search-for-primes-v2 a b)
  (define (prime-iter x y)
    (cond ((> x y)
           (display ""))
          (else
           (timed-prime-test-v2 x)
           (prime-iter (+ x 2) y))))
  (cond ((< (* a b) 0)
         (error "The interval [a,b] contains negative numbers!"))
        ((> a b)
         (search-for-primes-v2 b a))
        ((even? a)
         (search-for-primes-v2 (inc a) b))
        (else
         (prime-iter a b))))

;test 'search-for-primes-v2' for numbers 1000, 10000, 100000 and 1000000 
;---
(newline) (display "EXERCISE 1.23.") (newline)
;---
(display "- primes larger than 1000")
(newline) (search-for-primes-v2 1000 1050)
;---
(display "- primes larger than 10000")
(newline) (search-for-primes-v2 10000 10050)
;---
(display "- primes larger than 100000")
(newline) (search-for-primes-v2 100000 100050)
;---
(display "- primes larger than 1000000")
(newline) (search-for-primes-v2 1000000 1000050)

;results
;---
;3 primes larger than 1000
;v1: 1009 *** 2 | 1013 *** 2 | 1019 *** 2 -> average = 2 microseconds
;v2: 1009 *** 1 | 1013 *** 1 | 1019 *** 2 -> average = 1.33 microseconds
;---
;3 primes larger than 10000
;v1: 10007 *** 5 | 10009 *** 5 | 10037 *** 4 -> average = 4.66 microseconds
;v2: 10007 *** 3 | 10009 *** 3 | 10037 *** 3 -> average = 3 microseconds
;---
;3 primes larger than 100000
;v1: 100003 *** 13 | 100019 *** 13 | 100043 *** 13 -> average = 13 microseconds
;v2: 100003 *** 9 | 100019 *** 9 | 100043 *** 9 -> average = 9 microseconds
;---
;3 primes larger than 1000000
;v1: 1000003 *** 39 | 1000033 *** 39 | 1000037 *** 39 -> average = 39 microseconds
;v2: 1000003 *** 27 | 1000033 *** 27 | 1000037 *** 26 -> average = 26.66 microseconds

;do the results support the prediction that runtime(v1)=2*runtime(v2)?
;---
(display "- checking the runtime(v1)=2*runtime(v2) prediction") (newline)
;---
;1000
(/ 2 1.33) ;1.50
;---
;10000
(/ 4.66 3) ;1.55
;---
;100000
(/ 13.0 9) ;1.44
;---
;1000000
(/ 39 26.66) ;1.46

;conclusion
;---
;the prediction that runtime(v1)=2*runtime(v2) does not hold true. instead of a speed
;ratio of 2 between the two algorithms (v1/v2), a ratio of approximately 1.5 was
;attained. this might be due to an extra call to an helper procedure ('next') in the
;'find-divisor-v2' procedure. inlining 'next' in 'find-divisor-v2' (that is, writting the
;body of 'next' in 'find-divisor-v2') might yield results closer to the prediction that
;runtime(v1)=2*runtime(v2). for extra detail, refer to Sébastien Gignoux's solution:
;https://sicp-solutions.net/post/sicp-solution-exercise-1-23/


;------------------------------------------------------------------------------------------
;EXERCISE 1.24.
;---
;Modify the 'timed-prime-test' procedure of Exercise 1.22 to use 'fast-prime'? (the
;Fermat method, SEE BELOW ***), and test each of the 12 primes you found in that
;exercise. Since the Fermat test has O(log(n)) growth, how would you expect the time to
;test primes near 1,000,000 to compare with the time needed to test primes near 1000? Do
;your data bear this out? Can you explain any discrepancy you find?
;------------------------------------------------------------------------------------------

;Fermat's Little Theorem
;---
;If 'n' is a prime number and 'a' is any positive integer less than 'n', then 'a' raised
;to the nth power is congruent to 'a' modulo 'n'. ["Two numbers are said to be congruent
;modulo 'n' if they both have the same remainder when divided by 'n'. The remainder of a
;number 'a' when divided by 'n' is also referred to as the remainder of 'a' modulo 'n',
;or simply as 'a' modulo 'n'."]

;helper procedures
;---
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
;---
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;(***) 'fast-prime?' procedure
;---
;"If 'n' is not prime, then, in general, most of the numbers a < n will not satisfy the
;above relation. This leads to the following algorithm for testing primality: Given a
;number 'n', pick a random number a < n and compute the remainder of a^n modulo 'n'. If
;the result is not equal to 'a', then 'n' is certainly not prime. If it is 'a', then
;chances are good that 'n' is prime. Now pick another random number 'a' and test it with
;the same method. If it also satisfies the equation, then we can be even more confident
;that 'n' is prime. By trying more and more values of 'a', we can increase our confidence
;in the result. This algorithm is known as the Fermat test."
;---
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;redifining 'start-prime-test' -> 'start-prime-test-v3'
;default number of times the test is ran -> 'times'=4
;---
(define (start-prime-test-v3 n start-time)
  (if (fast-prime? n 4)
      (report-prime (- (runtime) start-time))))

;redifining 'timed-prime-test' -> 'timed-prime-test-v3'
;---
(define (timed-prime-test-v3 n)
  (display n)
  (start-prime-test-v3 n (runtime))
  (newline))

;redifining 'search-for-primes' -> 'search-for-primes-v3'
;---
(define (search-for-primes-v3 a b)
  (define (prime-iter x y)
    (cond ((> x y)
           (display ""))
          (else
           (timed-prime-test-v3 x)
           (prime-iter (+ x 2) y))))
  (cond ((< (* a b) 0)
         (error "The interval [a,b] contains negative numbers!"))
        ((> a b)
         (search-for-primes-v3 b a))
        ((even? a)
         (search-for-primes-v3 (inc a) b))
        (else
         (prime-iter a b))))

;test 'search-for-primes-v3' for numbers 1000, 10000, 100000 and 1000000 
;---
(newline) (display "EXERCISE 1.24.") (newline)
;---
(display "- primes larger than 1000")
(newline) (search-for-primes-v3 1000 1050)
;---
(display "- primes larger than 10000")
(newline) (search-for-primes-v3 10000 10050)
;---
(display "- primes larger than 100000")
(newline) (search-for-primes-v3 100000 100050)
;---
(display "- primes larger than 1000000")
(newline) (search-for-primes-v3 1000000 1000050)

;results
;---
;3 primes larger than 1000
;1009 *** 7 | 1013 *** 5 | 1019 *** 5 -> average = 5.33 microseconds
;---
;3 primes larger than 10000
;10007 *** 6 | 10009 *** 5 | 10037 *** 8 -> average = 6.33 microseconds
;---
;3 primes larger than 100000
;100003 *** 9 | 100019 *** 6 | 100043 *** 6 -> average = 7 microseconds
;---
;3 primes larger than 1000000
;1000003 *** 7 | 1000033 *** 8 | 1000037 *** 7 -> average = 7.33 microseconds

;conclusion
;---
;diff(10000, 1000) = 6.33 - 5.33 = 1.0
;diff(100000, 10000) = 7 - 6.33 = 0.67
;diff(1000000, 100000) = 7.33 - 7 = 0.33
;---
;to state that 'fast-prime?' has an order of growth of O(log(n)), it would probably be
;beneficial to perform more tests for larger numbers (e.g., 10^7, 10^8, etc.). however,
;the results suggest that there is a constant decrease in speed as the order of magnitude
;of n increases. therefore, the prediction that 'fast-prime?' as an order of growth of
;O(log(n)) holds true


;------------------------------------------------------------------------------------------
;EXERCISE 1.25.
;---
;Alyssa P. Hacker complains that we went to a lot of extra work in writing 'expmod'.
;After all, she says, since we already know how to compute exponentials, we could have
;simply written (SEE BELOW ****). Is she correct? Would this procedure serve as well for
;our fast prime tester? Explain.
;------------------------------------------------------------------------------------------

;helper procedure
;---
(define (fast-expt b e)
  (cond ((= e 0) 1)
        ((even? e) (square (fast-expt b (/ e 2))))
        (else (* b (fast-expt b (dec e))))))

;(****) alternative procedure for 'expmod' -> 'expmod-v2'
;---
(define (expmod-v2 base exp m)
  (remainder (fast-expt base exp) m))

;footnote 46 from textbook
;---
;"The reduction steps in the cases where the exponent 'e' is greater than 1 are based on
;the fact that, for any integers 'x', 'y', and 'm', we can find the remainder of 'x'
;times 'y' modulo 'm' by computing separately the remainders of 'x' modulo 'm' and 'y'
;modulo 'm', multiplying these, and then taking the remainder of the result modulo 'm'.
;For instance, in the case where 'e' is even, we compute the remainder of b^(e/2) modulo
;'m', square this, and take the remainder modulo 'm'. This technique is useful because it
;means we can perform our computation without ever having to deal with numbers much
;larger than 'm'. (Compare Exercise 1.25.)"

;ANSWER to exercise
;---
(newline) (display "EXERCISE 1.25.") (newline)
;---
;when performing the fermat's test on a somewhat large number 'n', the result of
;'fast-expt' may become very large if the randomly chosen value of 'a' (a < n) is not
;small. for example, consider the test for n = 199. when performing the test, a random
;numer 'a' is chosen. assume it to be 140. here, 'fast-expt' yields a number which is
;428 digits long. as we test larger and larger numbers, this large intermediate result
;might become problematic in terms of memory usage (+). on the other hand, by
;implementing 'expmod' as in exercise 1.24., "we can perform our computation without ever
;having to deal with numbers much larger than 'n'". in fact, 'remainder' only has to
;deal with numbers no larger than (n-1)^2 (++)
;---
;(+)
(fast-expt 140 199)
;---
;(++)
;(expmod 140 199 199)
;* (expmod 140 198 199)
;* sq (expmod 140 99 199)
;* sq * (expmod 140 98 199)
;* sq * sq (expmod 140 49 199)
;* sq * sq * (expmod 140 48 199)
;* sq * sq * sq (expmod 140 24 199)
;* sq * sq * sq sq (expmod 140 12 199)
;* sq * sq * sq sq sq (expmod 140 6 199)
;* sq * sq * sq sq sq sq (expmod 140 3 199)
;* sq * sq * sq sq sq sq * (expmod 140 2 199)
;* sq * sq * sq sq sq sq * sq (expmod 140 1 199)
;* sq * sq * sq sq sq sq * (expmod 140 2 199)
;* sq * sq * sq sq sq sq * sq * (expmod 140 0 199)
;* sq * sq * sq sq sq sq * sq * 1
;* sq * sq * sq sq sq sq * sq (% (* 140 1) 199)
;* sq * sq * sq sq sq sq * sq 140
;* sq * sq * sq sq sq sq * (% (* 140 140) 199)
;* sq * sq * sq sq sq sq * 98
;* sq * sq * sq sq sq sq (% (* 140 98) 199)
;* sq * sq * sq sq sq sq 188
;* sq * sq * sq sq sq (% (* 188 188) 199)
;* sq * sq * sq sq sq 121
;* sq * sq * sq sq (% (* 121 121) 199)
;* sq * sq * sq sq 114
;* sq * sq * sq (% (* 114 114) 199)
;* sq * sq * sq 61
;* sq * sq * (% (* 61 61) 199)
;* sq * sq * 139
;* sq * sq (% (* 140 139) 199)
;* sq * sq 157
;* sq * (% (* 157 157) 199)
;* sq * 172
;* sq (% (* 140 172) 199)
;* sq 1
;* (% (* 1 1) 199)
;* 1
;(% (* 140 1) 199)
;140


;------------------------------------------------------------------------------------------
;EXERCISE 1.26.
;---
;Louis Reasoner is having great difficulty doing Exercise 1.24. His 'fast-prime?' test
;seems to run more slowly than his 'prime?' test. Louis calls his friend Eva Lu Ator over
;to help. When they examine Louis's code, they find that he has rewritten the 'expmod'
;procedure to use an explicit multiplication, rather than calling 'square' (SEE BELOW
;*****). "I don't see what difference that could make", says Louis. "I do", says Eva. "By
;writing the procedure like that, you have transformed the O(log(n)) process into a O(n)
;process". Explain.
;------------------------------------------------------------------------------------------

;(*****) alternative procedure for 'expmod' -> 'expmod-v3'
;---
(define (expmod-v3 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod-v3 base (/ exp 2) m)
                       (expmod-v3 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base
                       (expmod-v3 base (- exp 1) m))
                    m))))

;ANSWER to exercise
;---
;Lisp's interpreter uses applicative-order evaluation, that is, the operands are
;evaluated before applying the operator. hence, by explicitly multiplying instead of
;squaring, the combination (expmod base (/ exp 2) m) is evaluated 2 times. thus, the
;benefits of squaring the result are lost (by using 'square', (expmod base (/ exp 2) m)
;is evaluated 1 time and the result is then squared). this doubles the work to be done
;at every call to 'expmod' (when 'exp' is even), that is, the order of growth becomes
;O(log(2^n)) = O(n*log(2)) = O(n)


;------------------------------------------------------------------------------------------
;EXERCISE 1.27.
;---
;Demonstrate that the Carmichael numbers listed in Footnote 1.47 (SEE BELOW ******)really
;do fool the Fermat test. That is, write a procedure that takes an integer 'n' and tests
;whether an is congruent to 'a' modulo 'n' for every a < n, and try your procedure on the
;given Carmichael numbers.
;------------------------------------------------------------------------------------------

;(******) footnote 47
;---
;"Numbers that fool the Fermat test are called Carmichael numbers, and little is known
;about them other than that they are extremely rare. There are 255 Carmichael numbers
;below 100,000,000. The smallest few are 561, 1105, 1729, 2465, 2821, and 6601. In
;testing primality of very large numbers chosen at random, the chance of stumbling upon a
;value that fools the Fermat test is less than the chance that cosmic radiation will
;cause the computer to make an error in carrying out a "correct" algorithm. Considering
;an algorithm to be inadequate for the first reason but not for the second illustrates
;the difference between mathematics and engineering."

;ANSWER to exercise -> 'exhaustive-fermat'
;---
(define (exhaustive-fermat n)
  (define (fermat-test a) (= (expmod a n n) a))
  (define (fermat-iter f)
    (cond ((= f 0) true)
          ((fermat-test f) (fermat-iter (dec f)))
          (else false)))
  (fermat-iter (dec n)))

;test for Carmichael numbers in footnote 47
;---
(newline) (display "EXERCISE 1.27.") (newline)
;---
(define (test-27 n)
  (display n) (display " ")
  (display (exhaustive-fermat n))
  (newline))
;---
(test-27 561)
(test-27 1105)
(test-27 1729)
(test-27 2465)
(test-27 2821)
(test-27 6601)


;------------------------------------------------------------------------------------------
;EXERCISE 1.28.
;---
;One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test
;(Miller 1976; Rabin 1980). This starts from an alternate form of Fermat’s Little
;Theorem, which states that if 'n' is a prime number and 'a' is any positive integer less
;than 'n', then 'a' raised to the (n-1)st power is congruent to 1 modulo 'n'. To test the
;primality of a number 'n' by the Miller-Rabin test, we pick a random number a < n and
;raise a to the (n-1)st power modulo 'n' using the 'expmod' procedure. However, whenever
;we perform the squaring step in 'expmod', we check to see if we have discovered a
;"nontrivial square root of 1 modulo n", that is, a number not equal to 1 or n-1 whose
;square is equal to 1 modulo 'n'. It is possible to prove that if such a nontrivial
;square root of 1 exists, then 'n' is not prime. It is also possible to prove that if 'n'
;is an odd number that is not prime, then, for at least half the numbers a < n, computing
;a^(n-1) in this way will reveal a nontrivial square root of 1 modulo 'n'. (This is why
;the Miller-Rabin test cannot be fooled.) Modify the expmod procedure to signal if it
;discovers a nontrivial square root of 1, and use this to implement the Miller-Rabin test
;with a procedure analogous to 'fermat-test'. Check your procedure by testing various
;known primes and non-primes. Hint: One convenient way to make expmod signal is to have
;it return 0.
;------------------------------------------------------------------------------------------

;prime(n) => a^(n-1) congruent to 1 (modulo n)
;---
;test: (= (expmod a (dec n) n) 1) ???
;---
;extra step: when performing 'square' in 'expmod' check if a "nontrivial square root of 1
;modulo n" (a number not equal to 1 or n-1, whose square is equal to 1 modulo n) exists.
;if such a number exists, then n is not prime

;check whether a nontrivial square root of 1 modulo n exists
;---
(define (inspect-expmod a n)
  (define r (remainder (square a) n))
  (if (and (not (= a (dec n))) (not (= a 1)))
      (if (= r 1) 0 r)
      r))

;redefining 'expmod' -> 'expmod-mr'
;---
(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (inspect-expmod (expmod-mr base (/ exp 2) m) m))
        (else
         (remainder
          (* base (expmod-mr base (- exp 1) m))
          m))))

;'miller-rabin-test'
;---
(define (miller-rabin-test n times)
  (define (try-it a)
    (= (expmod-mr a (dec n) n) 1))
  (define (get-random)
    (+ 1 (random (- n 1))))
  (define (mr-iter count)
    (cond ((> count times) true)
          ((try-it (get-random)) (mr-iter (inc count)))
          (else false)))
  (mr-iter 1))

;test for primality using 'miller-rabin-test'
;---
(newline) (display "EXERCISE 1.28.") (newline)
;---
(define (test-28 n)
  (display n) (display " ")
  (display (miller-rabin-test n 4))
  (newline))

;test for carmichael numbers in footnote 47
;---
(display "---") (newline)
(display "test for carmichael numbers") (newline)
(test-28 561)
(test-28 1105)
(test-28 1729)
(test-28 2465)
(test-28 2821)
(test-28 6601)

;test for some prime numbers
;---
(display "---") (newline)
(display "test for prime numbers") (newline)
(test-28 13)
(test-28 41)
(test-28 199)
(test-28 1999)

