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
