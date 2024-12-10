#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define v (list 1 2))

(define w 
  (list 
    (list 1 2)
    (list 3 4))
  )

;; (map * v w)
v
w

(accumulate + 0 (map * v w))
