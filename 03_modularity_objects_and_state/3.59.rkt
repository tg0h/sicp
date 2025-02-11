#lang sicp

(define (inverse x) (/ 1 x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (mul-streams s1 s2)
  (if (stream-null? s1) the-empty-stream
      (cons-stream (* (stream-car s1) (stream-car s2))
                   (mul-streams (stream-cdr s1) (stream-cdr s2)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (stream-map proc s)
  (if (stream-null? s) the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define inverse-fractions (stream-map inverse (integers-starting-from 1 )))

(stream-ref inverse-fractions 0)
(stream-ref inverse-fractions 1)
(stream-ref inverse-fractions 2)
(stream-ref inverse-fractions 3)

(define (integrate-series s c)
  (cons-stream c (mul-streams inverse-fractions s))
  )
