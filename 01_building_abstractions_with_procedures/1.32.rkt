#lang sicp

(define (identity x) x)
(define (next x) (+ x 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result)))
    )
  (iter a 0)
  )

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result)))
    )
  (iter a 1)
  )

(sum identity 1 next 4)
(product identity 1 next 4)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result)))
    )
  (iter a null-value)
  )

(define (plus x y ) (+ x y))

(accumulate + 0 identity 1 next 3)
(accumulate + 0 identity 1 next 4)
(accumulate * 1 identity 1 next 3)
(accumulate * 1 identity 1 next 4)
