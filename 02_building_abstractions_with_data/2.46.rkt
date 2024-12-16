#lang sicp


(define (make-vect x y)
  (cons x y)
  )

(define (xcor-vect v)
  (car v)
  )


(define (ycor-vect v)
  (cdr v)
  )

(define v (make-vect 1 2))
(xcor-vect v)
(ycor-vect v)

