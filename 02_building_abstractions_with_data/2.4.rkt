#lang sicp

(define (cons x y)
  (lambda (m) (m x y))
  )

(define (car z)
  (z (lambda (p q) p)))

(define gg (cons 1 2))
(car gg)
