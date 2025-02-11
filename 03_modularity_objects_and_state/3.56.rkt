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

;; (define (stream-map proc s)
;;   (if (stream-null? s) the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))

(define
  (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams
                (partial-sums s)
                (stream-cdr s))
               )
  )

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; (stream-car (partial-sums integers))
;; (stream-cdr (partial-sums integers))
(stream-ref (partial-sums integers) 0)
(stream-ref (partial-sums integers) 1)
(stream-ref (partial-sums integers) 2)
(stream-ref (partial-sums integers) 3)
