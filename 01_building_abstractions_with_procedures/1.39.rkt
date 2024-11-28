#lang sicp

(define (cont-frac-recur n d k)
  (define (recur i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ 1 i))))))
  (recur 1))

(define (n i) 1.0)

(define (d i)
  (
   cond ((= (remainder i 3) 1) 1)
        ((= (remainder i 3) 0) 1)
        (else (* 2 (/ (+ i 1) 3)))
        )
  )

(cont-frac-recur n d 1)
(cont-frac-recur n d 5)
(cont-frac-recur n d 10)
(cont-frac-recur n d 50)
(cont-frac-recur n d 100)
