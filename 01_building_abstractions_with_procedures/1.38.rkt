#lang sicp

(define (cont-frac n d k)
  (if (= k 1) 1
      (/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))
      )
  )
