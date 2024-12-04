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

;; (print-point (make-point 1 2))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (average a b) (/ (+ a b) 2))

(define (mid-point-segment s)
  (let
      (
       (start-point (start-segment s))
       (end-point (end-segment s))
       )
    (make-point
     (average (x-point start-point) (x-point end-point))
     (average (y-point start-point) (y-point end-point))
     )
    )
  )
;; (make-segment (make-point 1 2) (make-point 3 4))

(print-point (mid-point-segment (make-segment (make-point 1 2) (make-point 3 4))))
