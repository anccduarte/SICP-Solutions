
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.36.
;---
;Suppose we evaluate the following sequence of expressions in the global environment:
;---
;(define a (make-connector))
;(define b (make-connector))
;(set-value! a 10 'user)
;---
;At some time during evaluation of the 'set-value!', the following expression from the
;connector's local procedure is evaluated:
;---
;(for-each-except
; setter inform-about-value constraints)
;---
;Draw an environment diagram showing the environment in which the above expression is
;evaluated.
;------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------
;DEFINING PROCEDURES IN GLOBAL ENVIRONMENT
;------------------------------------------------------------------------------------------
;(define (for-each-except <params1>) <body1>)
;(define (make-connector) <body2>)
;(define (set-value! <params2>) <body3>)
;---
;                         +-----------------------------+
;                         ↓                             |
;             +---------------------------+       +---+-|-+
; global ---> | for-each-except: ---------------> | | | | |
;    env      | make-connector: ----+     |       +-|-+---+
;             | set-value!: ----+   |     |         ↓
;             +-----------------|---|-----+       params: <params1>
;                  ↑            |   |   ↑         body: <body1>
;              +----------------+   |   |
;              |   |                |   |
;              ↓   |                ↓   |
;            +---+-|-+            +---+-|-+
;            | | | | |            | | | | |
;            +-|-+---+            +-|-+---+
;              ↓                    ↓
;            params: <params2>   params: '()
;            body: <body3>       body: <body2>

;------------------------------------------------------------------------------------------
;DEFINING CONNECTOR
;------------------------------------------------------------------------------------------
;(define a (make-connector))
;---
;                   +------------------------+
; global ---------> | for-each-except: (...) |
;    env            | make-connector: (...)  |
;                   | set-value!: (...)      |
;                   | a: ---+                |
;                   +-------|----------------+
;                           |         ↑
;            +--------------+         | 
;            |                        |  
;            ↓                  +------------------+
;          +---+---+            | value: false     |
;    +---> | | | -------------> | informant: false |
;    |     +-|-+---+            | constraints: '() |
;    |       ↓                  +------------------+
;    |     params: request            ↑
;    |     body: <body-me>            |
;    |                                |
;    |                          +----------------------+
;    |                          | set-my-value!: (...) |
;    |                          | forget!: (...)       |
;    |                          | connect: (...)       |
;    |                          | me: ---+             |
;    |                          +--------|-------------+
;    |                                   |
;    +-----------------------------------+

;------------------------------------------------------------------------------------------
;SETTING CONNECTOR'S VALUE (calls)
;------------------------------------------------------------------------------------------
;(set-value! a 10 'user)
;---
;                   +------------------------+
; global ---------> | for-each-except: (...) |      +--------------------------+
;    env            | make-connector: (...)  | <--- | exception: 'user         |
;                   | set-value!: (...)      |      | proc: inform-about-value |
;                   | a: ---+                |      | lst: '()                 |
;                   +-------|----------------+      +--------------------------+
;                           |         ↑                                [call to
;            +--------------+         |                       'for-each-except']
;            |                        |
;            ↓                  +------------------+
;          +---+---+            | value: false     |
;    +---> | | | -------------> | informant: false |
;    |     +-|-+---+            | constraints: '() |
;    |       ↓                  +------------------+
;    |     params: request            ↑
;    |     body: <body-me>            |
;    |                                |
;    |                          +----------------------+
;    |                          | set-my-value!: (...) |      +---------------+
;    |                          | forget!: (...)       | <--- | new-value: 10 |
;    |                          | connect: (...)       |      | setter: 'user |
;    |                          | me: ---+             |      +---------------+
;    |                          +--------|-------------+              [call to
;    |                                   |                     'set-my-value!']
;    +-----------------------------------+

;------------------------------------------------------------------------------------------
;SETTING CONNECTOR'S VALUE (result)
;------------------------------------------------------------------------------------------
;(set-value! a 10 'user)
;---
;                   +------------------------+
; global ---------> | for-each-except: (...) |
;    env            | make-connector: (...)  |
;                   | set-value!: (...)      |
;                   | a: ---+                |
;                   +-------|----------------+
;                           |         ↑
;            +--------------+         | 
;            |                        |  
;            ↓                  +------------------+
;          +---+---+            | value: 10        |
;    +---> | | | -------------> | informant: 'user |
;    |     +-|-+---+            | constraints: '() |
;    |       ↓                  +------------------+
;    |     params: request            ↑
;    |     body: <body-me>            |
;    |                                |
;    |                          +----------------------+
;    |                          | set-my-value!: (...) |
;    |                          | forget!: (...)       |
;    |                          | connect: (...)       |
;    |                          | me: ---+             |
;    |                          +--------|-------------+
;    |                                   |
;    +-----------------------------------+

