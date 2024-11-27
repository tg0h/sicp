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
    (display "guess:")
    (display guess)
    (newline)
    (let
        (
         (next (f guess))
         (average-damp-next (/ (+ guess  (f guess)) 2) )
         )
      (if (close-enough? guess next)
          average-damp-next
          (try average-damp-next)))
    )
  (try first-guess)
  )

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
