#lang sicp

(define (double f)
  (lambda (x) (f (f x)))
  )

(define (inc x)
  (+ x 1)
  )

;; (inc (inc 1) )
;; ((double inc) 1) ; adds 2 to a
(((double double) inc) 5)
(((double (double double)) inc) 5)
((double (double (double inc))) 5)
