
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.19.
;---
;There is a clever algorithm for computing the Fibonacci numbers in a logarithmic number
;of steps. Recall the transformation of the state variables a and b in the 'fib-iter'
;process: a <- a+b and b <- a. Call this transformation 'T', and observe that applying
;'T' over and over again n times, starting with 1 and 0, produces the pair Fib(n+1) and
;Fib(n). In other words, the Fibonacci numbers are produced by applying T^n , the nth
;power of the transformation 'T', starting with the pair (1, 0). Now consider 'T' to be
;the special case of p = 0 and q = 1 in a family of transformations T(pq), where T(pq)
;transforms the pair (a, b) according to a <- bq + aq + ap and b <- bp + aq. Show that if
;we apply such a transformation T(pq) twice, the effect is the same as using a single
;transformation T(p'q') of the same form, and compute p' and q' in terms of p and q. This
;gives us an explicit way to square these transformations, and thus we can compute T^n
;using successive squaring, as in the 'fast-expt' procedure. Put this all together to
;complete the following procedure, which runs in a logarithmic number of steps (SEE
;BELOW *).
;------------------------------------------------------------------------------------------

;(*) procedure for computing fib(n) in a logarithmic number of steps
;---
(define (fib n)
  (fib-iter 1 0 0 1 n))
;---
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)) ;compute p'
                   (+ (* q q) (* 2 p q)) ;compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;PROOF that p'=p^2+q^2 and q'=q^2+2pq
;---
;T -> transformation applied to the pair (a,b) such that a <- a+b and b <- a
;fib(n) = T^n(a,b) starting at (a,b) = (1,0)
;consider T to be a special case of a family of transformations T(pq) such that:
;a <- bq + aq + ap and b <- bp + aq, with p=0 and q=1.
;---
;[NOTE: at iteration 0 (a,b) = (1,0),
;at iteration 1, a <- 0*1 + 1*1 + 1*0 = 1 and b <- 0*0 + 1*1 = 1,
;at iteration 2, a <- 1*1 + 1*1 + 1*0 = 2 and b <- 1*0 + 1*1 = 1,
;at iteration 3, a <- 1*1 + 2*1 + 2*0 = 3 and b <- 1*0 + 2*1 = 2, ...]
;---
;the goal is to apply the transformation T(pq) twice and compute the values of p' and q'
;that allow to square these transformations and, thus, compute T^n in log(n)
;---
;one transformation (T^1)
;a <- bq + aq + ap and b <- bp + aq
;---
;two transformations (T^2)
;a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p =
;     = bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2 =
;     = b(pq + q^2 + pq) + a(pq + q^2 + pq) + a(p^2 + q^2) =
;     = b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2) =
;     = bq' + aq' + ap', with p' = p^2 + q^2 and q' = q^2 + 2pq
;b <- (bp + aq)p + (bp + aq + ap)q =
;     = bp^2 + apq + bq^2 + aq^2 + apq =
;     = b(p^2 + q^2) + a(q^2 + 2pq) =
;     = bp' + aq', with p' = p^2 + q^2 and q' = q^2 + 2pq
;---
;hence, the proof is complete

