#lang sicp

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))
    )
  )

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-limit s tolerance)
  (let (
        (next (stream-car (stream-cdr s)))
        (current (stream-car s)))
    (if (< (abs (- next current))  tolerance)
        next
        (stream-limit (stream-cdr s))))

  )
