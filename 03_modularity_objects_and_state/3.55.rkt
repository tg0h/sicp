#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (mul-streams s1 s2)
  (if (stream-null? s1) the-empty-stream
      (cons-stream (* (stream-car s1) (stream-car s2))
                   (mul-streams (stream-cdr s1) (stream-cdr s2)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define factorials (cons-stream 1
                                (mul-streams factorials (stream-cdr integers))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; (stream-cdr factorials)
(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
