#lang sicp

(define (f a b)
  (cond
    ((= a  b) 1)
    ((= a 1) 1)
    (else (+ (f (- a 1) (- b 1)) (f a (- b 1))))
    )
  )

(f 3 5)
