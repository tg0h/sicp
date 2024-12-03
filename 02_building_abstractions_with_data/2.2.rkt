#lang sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point point)
  (display "(")
  (display (car point))
  (display ",")
  (display (cdr point))
  (display ")")
  (newline)
  )

(print-point (make-point 1 2))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (average a b) (/ (+ a b) 2))

(define (mid-point-segment s)
  (let 
    ((start-point (start-segment s)))
    (start-point)
    ;; ((end-point (end-segment s)))
    ;; ((start-x (x-point start-point)))
    ;; ((start-y (y-point start-point)))
    ;; ((end-x (x-point end-point)))
    ;; ((end-y (y-point end-point)))
    ;; ((average-x (average start-x end-x)))
    ;; ((average-y (average start-y end-y)))
    ;; (make-point average-x average-y)
    )
  )

(mid-point-segment (make-segment (make-point 1 2) (make-point 3 4)))
