#lang sicp

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))

(define (add-vect v1 v2)
  (let
      (
       (x-v1 (xcor-vect v1))
       (y-v1 (ycor-vect v1))
       (x-v2 (xcor-vect v2))
       (y-v2 (ycor-vect v2))
       )
    (make-vect (+ x-v1 x-v2) (+ y-v1 y-v2))
    )
  )

;; (add-vect v1 v2)

(define (sub-vect v1 v2)
  (let
      (
       (x-v1 (xcor-vect v1))
       (y-v1 (ycor-vect v1))
       (x-v2 (xcor-vect v2))
       (y-v2 (ycor-vect v2))
       )
    (make-vect (- x-v1 x-v2) (- y-v1 y-v2))
    )
  )

;; (sub-vect v1 v2)

(define (scale-vect s v1)
  (let
      (
       (x-v1 (xcor-vect v1))
       (y-v1 (ycor-vect v1))
       )
    (make-vect (* s x-v1) (* s y-v1))
    )
  )

;; (scale-vect 2 v1)
