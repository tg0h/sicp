#lang sicp

(define dx 0.00001)

; deriv g returns a lambda
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
  )
