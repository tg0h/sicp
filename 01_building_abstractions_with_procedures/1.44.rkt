#lang sicp


(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (repeated f n)
  (define (iter i)
    (if (= i 1)
        f
        (compose f (iter (- i 1)))
        )
    )
  (iter n)
  )

(define dx 0.0001)

(define (smooth f)

  )
