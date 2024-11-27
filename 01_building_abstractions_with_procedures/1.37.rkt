#lang sicp

(define (cont-frac n d k )
  (define (iter n d k i result)
    (cond
      ((> i k ) result)
      (else (/ (n i) (d i)))
      )

    )
  (iter n d 3 1 1)
  )
