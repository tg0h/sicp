#lang sicp


(define (repeated f n)
  (define (iter i)
    (if (= i 1)
        f
        (f (iter (- i 1)))
        )
    )
  (iter n)
  )
