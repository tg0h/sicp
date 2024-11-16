#lang sicp

;; good enough
(define (square x) (* x x))
(define (cube x) (* (* x x) x))
(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))
    )
  )
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

;; improve
(define (average x y) (/ (+ x y) 2))
;; (define (improve guess x) (average guess (/ x guess)))
(define (improve guess x) (/ (+ (/ x (square guess)) (* 2 guess))  3 ))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
      )
  )

(define (cubert-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubert-iter (improve guess x) x)
      )
  )

(cubert-iter 1.0 8.0)

;; (improve 1 8)
;; (improve 10/3 8)
;; (improve 554/225 8)
