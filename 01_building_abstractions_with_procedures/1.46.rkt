#lang sicp

(define (iterative-improve good-enough-guess improve-guess)
  (lambda (guess)
    (define (iter guess)
      (if (good-enough-guess guess)
          guess
          (iter (improve-guess guess))
          )
      )
    (iter guess)
    )
  )

(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))

(define (square x) (* x x))
(define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))
(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))


