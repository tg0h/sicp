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
  (define (iter a b c k)
    (display a)
    (display ":" )
    (display b)
    (display ":" )
    (display c)
    (display ":" )
    (display k)
    (newline)
    (cond ((= b 1) (+ c k))
          ((even? b) (iter a (halve b) k (double a)) ); even
          (else (iter a (- b 1) k k)) ; odd
          )
    )
  (iter a b 0 1)
  )

(f-mult 3 2)
