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


(define
  (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x) (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)
