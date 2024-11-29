#lang sicp

(define tolerance 0.00001)

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))
    )
  )

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (average x y) (/ (+ x y) 2))

;; (fixed-point (lambda (x) (/ 1 x) ) 1.1)

(define (sqrt a) (fixed-point (lambda (x) (/ a x)) 1.0))

(define (power a n)
  (if (= n 1) a (* a (power a (- n 1)))
      )
  )

(power 2 5)

;; (define (root-n a)
;;   lambda (x) (/ a ()
;;                 )
;;
;;   (sqrt 2)
