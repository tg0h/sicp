#lang sicp

(define (cont-frac n d k)
  (if (= k 1) 1
      (/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))
      )
  )

(define (n i) 1.0)

(define (d i)
  (
   cond ((= (remainder i 3) 1) 1)
        ((= (remainder i 3) 0) 1)
        (else (* 2 (/ (+ i 1) 3)))
        )
  )
