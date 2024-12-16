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

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (org-frame f) (car f))
(define (e1-frame f) (cadr f))
(define (e2-frame f) (caddr f))

(define (_make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (_org-frame f) (car f))
(define (_e1-frame f) (cadr f))
(define (_e2-frame f) (cddr f))


(define org (make-vect 0 0))
(define e1 (make-vect 1 1))
(define e2 (make-vect -1 1))
(define f (make-frame org e1 e2))

f
(org-frame f)
(e1-frame f)
(e2-frame f)

(define _f (_make-frame org e1 e2))

_f
(_org-frame _f)
(_e1-frame _f)
(_e2-frame _f)
