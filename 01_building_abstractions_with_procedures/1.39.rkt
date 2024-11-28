#lang sicp

(define (cont-frac-recur n d k)
  (define (recur i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ 1 i))))))
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

(cont-frac-recur n d 1)
(cont-frac-recur n d 5)
(cont-frac-recur n d 10)
(cont-frac-recur n d 50)
(cont-frac-recur n d 100)
