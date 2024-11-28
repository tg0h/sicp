#lang sicp

(define dx 0.00001)
; deriv g returns a lambda
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
  )

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) (fixed-point (newton-transform g) guess))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))