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

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define inverse-fractions (stream-map inverse (integers-starting-from 1 )))

;; (stream-ref inverse-fractions 0)
;; (stream-ref inverse-fractions 1)
;; (stream-ref inverse-fractions 2)
;; (stream-ref inverse-fractions 3)

(define (integrate-series s)
  (mul-streams inverse-fractions s))


(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; (stream-ref exp-series 0)
;; (stream-ref exp-series 1)
;; (stream-ref exp-series 2)
;; (stream-ref exp-series 3)


(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))


(define sine-series (cons-stream 0 (integrate-series cosine-series)))

; deriv cosine is negative of sine
(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series) -1 )))


(define (add-streams s1 s2) (stream-map + s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (mul-series  (stream-cdr s) (invert-unit-series s)))
  )
