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

(define improve-guess-sqrt-2
  (lambda (guess)
    (average guess (/ 2 guess))))

(define (square x) (* x x))
(define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))

(define good-enough-sqrt-2?
  (lambda (guess)
    (< (abs (- (square guess) 2)) 0.001))
  )

;; ((iterative-improve good-enough-sqrt-2? improve-guess-sqrt-2) 1.0)

;; (define fixed-point-good-enough?
;;   (lambda (guess) )
;;   )


(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess)
  )

(define good-enough-fixed-cos?
  (lambda (guess)
    (< (abs (- guess (cos guess))) tolerance))
  )

(define improve-guess-sqrt-2
  (lambda (guess)
    (average guess (/ 2 guess))))


;; (fixed-point cos 1.0)
