#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (* n 2)
  )

(define (halve n)
  (/ n 2)
  )

(define (f-mult a b )
  (define (iter a b c product)
    (display a)
    (display ":" )
    (display b)
    (display ":" )
    (display c)
    (display ":" )
    (display product)
    (newline)
    (cond ((= b 1) (+ c product))
          ((even? b) (iter a (halve b) product (double a)) ); even
          (else (iter a (- b 1) product product)) ; odd
          )
    )
  (iter a b 0 1)
  )

(f-mult 3 2)
