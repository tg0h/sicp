#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (mul-streams s1 s2)
  (if (stream-null? s1) the-empty-stream
      (cons-stream
       (* (stream-car s1) (stream-car s2))
       (mul-streams (stream-cdr s1) (stream-cdr s2))
       )
      )
  )
