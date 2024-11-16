#lang sicp

;; good enough
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

(define (_good-enough? guess x)
  (< (/ (abs (- x (square guess) )) x) 0.01))

;; improve
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
      )
  )

;; (sqrt-iter 1 2)
;; (sqrt-iter 1 123456789)
;; (sqrt-iter 1.0 16e64)

;; (square 16e64)
;; (abs (- 1.0 (square 16e64)))


;; (/ 16e64 8e64)
(improve 8e64 16e64)
;; (improve 61728394.50000001 1.0)

;; (abs (- 16e64 (square 4.0e32)))
;; (square 4.0e32)

(abs (- 16e64 1.6e65))
(abs (- 16e64 (square 4.0e32)  ))
