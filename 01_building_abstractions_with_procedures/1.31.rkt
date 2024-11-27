#lang sicp


(define (next x) (+ x 1))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result)))
    )
  (iter a 1)
  )

(define (even-y x ) (* 2 x))
(define (odd-y x ) (+ 1 (* 2 x)))

(define (even-mult a b)
  (product even-y 1 next 3)
  )

;; (even-mult 1 3)

(define (odd-mult a b)
  (product odd-y 1 next 3)
  )

;; (odd-mult 1 3)

(define (pi-approx n)
  (*
   ( /
     (* (even-mult 1 n)
        (/ (even-mult 1 (+ 1 n) 2)))
     (* (odd-mult 1 n) (odd-mult 1 n))
     )
   4
   )
  )

(pi-approx 3)
