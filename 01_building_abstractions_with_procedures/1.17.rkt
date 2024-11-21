#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

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
    (cond ((= k 1) a)
          ((not (even? k)) (f-exp (* a b) b (- k 1)))
          (else (f-exp (* a (square b)) b (/ k 2)))
          )
    )
  )

(f-exp 1 2 5 )
