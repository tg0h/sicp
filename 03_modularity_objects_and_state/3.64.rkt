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
  (let ((next (stream-car (stream-cdr s)))
        (current (stream-car s)))
    (display "next: ")
    (display next)
    (newline)
    (display "current: ")
    (display current)
    (newline)
    (display (abs (- next current)))
    (newline)
    (newline)
    (if (< (abs (- next current))  tolerance)
        next
        (stream-limit (stream-cdr s) tolerance))))


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

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.000000000000000000000000000001)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(stream-ref (sqrt-stream 2) 0)
(stream-ref (sqrt-stream 2) 1)
(stream-ref (sqrt-stream 2) 2)
(stream-ref (sqrt-stream 2) 3)
(stream-ref (sqrt-stream 2) 4)
