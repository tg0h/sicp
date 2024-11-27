#lang sicp

(define (sum term a next b)
  (if (> a b) 0 (+ (term a) (sum term (next a) next b))))

(define (cube x ) (* (* x x) x))
(define (y x ) x)

(define (next x) (+ x 1))

(sum cube 1 next 9)

(define (_sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result)))
    )
  (iter a 0)
  )

(_sum cube 1 next 9)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result)))
    )
  (iter a 1)
  )

(product y 1 next 3)

(define (factorial n)
  (product y 1 next n)
  )

(factorial 5)
