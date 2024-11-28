#lang sicp

(define (tan-cf x k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n x i) (- (d i) (recur (+ 1 i))))))
  (recur 1))

(define (n x i)
  (
   cond ((= i 1) x)
        (else (* x x ))
        )
  )

(define (d i)
  (+ 1 (* (- i 1) 2))
  )

(tan-cf 1.0 1)
(tan-cf 1.0 5)
(tan-cf 1.0 10)
(tan-cf 1.0 100)
