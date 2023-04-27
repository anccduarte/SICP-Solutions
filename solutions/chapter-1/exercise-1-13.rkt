
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.13.
;---
;Prove that Fib(n) is the closest integer to φ^n/sqrt(5), where φ = (1+sqrt(5))/2. Hint:
;Let ψ = (1-sqrt(5))/2. Use induction and the definition of the Fibonacci numbers to
;prove that Fib(n) = (φ^n-ψ^n)/sqrt(5).
;------------------------------------------------------------------------------------------

;proof heavily inspired by the work of Sébastien Gignoux
;https://sicp-solutions.net/post/sicp-solution-exercise-1-13/


;---
;PART 1
;proof, by mathematical induction, that fib(n) = (φ^n-ψ^n)/sqrt(5), with φ = (1+sqrt(5))/2
;and ψ = (1-sqrt(5))/2
;---

;theorem: the identity fib(n)=(φ^n-ψ^n)/sqrt(5) holds for all n ∈ N
;---

;proof: by mathematical induction
;---
;let n=0. then, fib(0) = (φ^0-ψ^0)/sqrt(5) = (1-1)/sqrt(5) = 0 (correct)
;let n=1. then, fib(1) = (φ^1-ψ^1)/sqrt(5) = ((1+sqrt(5))/2 - (1-sqrt(5))/2) / sqrt(5) =
;                      = ((1-1+sqrt(5)+sqrt(5))/2) / sqrt(5) = (2*sqrt(5)/2) / sqrt(5) =
;                      = sqrt(5) / sqrt(5) = 1 (correct)
;hence, the identity holds for n=0 and n=1 (in order for the proof to be complete, we
;must have 2 base cases, since fib(n) is computed in terms of the 2 previous fibonacci
;numbers)
;---
;since fib(n) = fib(n-1) + fib(n-2), and assuming that the identity holds for all n ∈ N,
;then, by the induction hypothesis:
;fib(n) = fib(n-1) + fib(n-2) =>
;=> (φ^n-ψ^n)/sqrt(5) = (φ^n-1-ψ^n-1)/sqrt(5) + (φ^n+1-ψ^n+1)/sqrt(5) =>
;=> φ^n - ψ^n = φ^n-1 - ψ^n-1 + φ^n-2 - ψ^n-2 =>
;=> φ^n - ψ^n = φ^n/φ + φ^n/φ^2 - ψ^n/ψ - ψ^n/ψ^2 =>
;=> φ^n - ψ^n = φ^n * (1/φ + 1/φ^2) - ψ^n * (1/ψ + 1/ψ^2) =>
;(substituting φ by (1+sqrt(5))/2 and ψ by (1-sqrt(5))/2)
;=> φ^n - ψ^n = φ^n * (2/(1+sqrt(5)) + 4/(1+sqrt(5))^2) - ψ^n * (2/(1-sqrt(5)) + 4/(1-sqrt(5))^2) =>
;=> φ^n - ψ^n = φ^n * (2*(1+sqrt(5)) + 4) / (1+sqrt(5))^2 - ψ^n * (2*(1-sqrt(5)) + 4) / (1-sqrt(5))^2 =>
;=> φ^n - ψ^n = φ^n * (6+2*sqrt(5))/(6+2*sqrt(5)) - ψ^n * (6-2*sqrt(5))/(6-2*sqrt(5)) =>
;=> φ^n - ψ^n = φ^n * 1 - ψ^n * 1 => φ^n - ψ^n = φ^n - ψ^n
;---
;hence, by mathematical induction, the identity holds for all n ∈ N. thus, fib(n)=(φ^n-ψ^n)/sqrt(5),
;with φ=(1+sqrt(5))/2 and ψ=(1-sqrt(5))/2


;---
;PART 2
;proof that fib(n) is the closest integer to φ^n/sqrt(5), with φ=(1+sqrt(5))/2
;---

;theorem: fib(n) is the closest integer to φ^n/sqrt(5), with φ=(1+sqrt(5))/2
;---

;proof
;---
;as previously proved, fib(n)=(φ^n-ψ^n)/sqrt(5). the identity can be rewritten as
;fib(n) = φ^n/sqrt(5) - ψ^n/sqrt(5) => φ^n/sqrt(5) = fib(n) + ψ^n/sqrt(5) (*)
;---
;note that ψ=(1-sqrt(5))/2 is a real number greater than -1 and smaller than 0:
;-1 < ψ < 0. thus, -1 < ψ^n < 1. dividing all expressions by sqrt(5) yields
;-1/sqrt(5) < ψ^n/sqrt(5) < 1/sqrt(5). and since sqrt(5) > 2, the following holds:
;-1/2 < -1/sqrt(5) < ψ^n/sqrt(5) < 1/sqrt(5) < 1/2. so, -1/2 < ψ^n/sqrt(5) < 1/2. addiing
;fib(n) to all expressions yields: fib(n) - 1/2 < fib(n) + ψ^n/sqrt(5) < fib(n) + 1/2
;---
;(*) substituting fib(n) + ψ^n/sqrt(5) by φ^n/sqrt(5), fib(n) - 1/2 < φ^n/sqrt(5) < fib(n) + 1/2.
;hence, φ^n/sqrt(5) is contained in the interval (fib(n)-1/2, fib(n)+1/2), which means that
;the furthest it can be from fib(n) is some distance less than 1/2. so, indeed, fib(n) is the
;closest integer to φ^n/sqrt(5)
;---
;thus, the proof is complete

