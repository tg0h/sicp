#lang sicp

(define (mul-streams s1 s2)
  (if (stream-null? s1) the-empty-stream
      (cons-stream
       (* (stream-car s1) (stream-car s2))
       (mul-streams (steam-cdr s1) (stream-cdr s2))
       )
      )
  )
