
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.79.
;---
;Generalize the 'solve-2nd' procedure of Exercise 3.78 so that it can be used to solve
;general second-order differential equations d^2(y)/dt^2 = f(dy/dt, y).
;------------------------------------------------------------------------------------------

;Schematization of an "analog computer circuit" that solves the above equation
;---
; +----------------------------------------+
; |                              y0        |
; |                              ¦         |
; |                              ↓         |
; |     +--------+   ddy    +----------+   |  y
; +---> | map: f | -------> | integral | --+------>
; |     +--------+          +----------+
; |         |
; |         |     ddy       +----------+
; |         +-------------> | integral | --+------>
; |                         +----------+   |  dy
; |                              ↑         |
; |                              ¦         |
; |                             dy0        |
; +----------------------------------------+


;Generic 'solve-2nd'
;---
;Note that, in the definition of 'ddy', the more generic version of 'stream-map' is
;exploited. Moreover, is it the case that dy has to be scaled by 1/dt before being fed
;to 'map' as suggested in http://community.schemewiki.org/?sicp-ex-3.79 by Eva Lu Ator?
;In the next subsection, an alternative depiction of the circuit as well as the procedure
;implementing it are provided.
;---
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay ddy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;Alternative schematization
;---
; +----------------------------------------+
; |                              y0        |
; |                              ¦         |
; |                              ↓         |
; |     +--------+   ddy    +----------+   |  y
; +---> | map: f | -------> | integral | --+------>
; |     +--------+          +----------+
; |         |
; |         |     ddy       +----------+
; |         +-------------> | integral | --+------>
; |                         +----------+   |  dy
; |                              ↑         |
; |                              ¦         |
; |       +-------------+       dy0        |
; +------ | scale: 1/dt | <----------------+
;         +-------------+

;Alternative 'solve-2nd'
;---
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay ddy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f (scale-stream dy (/ 1 dt)) y))
  y)

