#lang sicp

(define dx 0.00001)
; deriv g returns a lambda
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
  )

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x))))
  )

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

(define (fixed-point-of-transform g transform guess) (fixed-point (transform g) guess))

(define (square x ) (* x x ))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0)
  )

(define (cubic a b c)
  (lambda (x)
    (+
     (+
      (+
       (* (* x x ) x)
       (* (* x x ) a)
       )
      (* x b)
      )
     c
     )
    )
  )
(newtons-method (cubic 1 1 1) 1)

