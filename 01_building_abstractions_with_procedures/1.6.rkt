#lang sicp
;; (#%require "./lib.rkt")

(define (new-if predicate then-clause else-clause) 
  (cond 
    (predicate then-clause)
    (else else-clause)
  )
)

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)


(define (square x) (* x x))
(define (abs x)
  (cond 
      ((> x 0) x)
      ((= x 0) 0)
      ((< x 0) (- x))
  )
)

(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.001))

(define (improve guess x) (average guess (/ x guess)))

(define (sqrt-iter guess x) 
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x))
)

(sqrt-iter 1 2)
