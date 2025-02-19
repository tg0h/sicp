#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define
  (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (add-streams s1 s2) (stream-map + s1 s2))
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s) (stream-cdr s))))

;; (stream-ref (ln-summands 1) 0)
;; (stream-ref (ln-summands 1) 1)
;; (stream-ref (ln-summands 1) 2)
;; (stream-ref (ln-summands 1) 3)
;; (stream-ref (ln-summands 1) 4)

(define ln-stream (partial-sums (ln-summands 1)) )

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-line x) (display x)(newline))
(define (display-stream s) (stream-for-each display-line s))

(define (square x) (* x x))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(stream-ref ln-stream 0)
(stream-ref ln-stream 1)
(stream-ref ln-stream 2)
(stream-ref ln-stream 3)
(stream-ref ln-stream 4)
(newline)

(define eln-stream (euler-transform ln-stream))

(stream-ref eln-stream 0)
(stream-ref eln-stream 1)
(stream-ref eln-stream 2)
(stream-ref eln-stream 3)
(stream-ref eln-stream 4)
(newline)

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))


(define eeln-stream (accelerated-sequence euler-transform ln-stream))

(stream-ref eeln-stream 0)
(stream-ref eeln-stream 1)
(stream-ref eeln-stream 2)
(stream-ref eeln-stream 3)
(stream-ref eeln-stream 4)
