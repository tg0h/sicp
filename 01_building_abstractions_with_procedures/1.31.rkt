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
  (product even-y 1 next b)
  )

;; (even-mult 1 3)

(define (odd-mult a b)
  (product odd-y 1 next b)
  )

;; (odd-mult 1 3)

(define (pi-approx n)
  (*
   ( /
     (* (even-mult 1.0 n)
        (/ (even-mult 1.0 (+ 1.0 n)) 2))
     (* (odd-mult 1.0 n) (odd-mult 1.0 n))
     )
   4.0)
  )

(even-mult 1 3)
(even-mult 1 4)
;; (even-mult 1 5)
(odd-mult 1 3)

(pi-approx 4)
(pi-approx 5)
(pi-approx 100)
(pi-approx 1000)
