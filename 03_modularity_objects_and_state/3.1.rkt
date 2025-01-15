#lang sicp

(define (make-accumulator initial)
  (define (add number)
    (set! initial (+ initial number))
    initial
    )
  add
  )

(define A (make-accumulator 5))

(A 10)
(A 10)


