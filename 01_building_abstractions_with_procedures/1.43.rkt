#lang sicp

(define (square x) (* x x))

(define (repeated f n)
  (define (iter i)
    (if (= i 1)
        f
        (f (iter (- i 1)))
        )
    )
  (iter n)
  )


;; ((repeated square 1) 2)
((repeated square 2) 2)
