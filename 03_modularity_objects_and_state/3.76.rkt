#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (make-zero-crossings input-stream last-smooth-value last-sense-value)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-sense-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt last-smooth-value)
     (make-zero-crossings
      (stream-cdr input-stream) avpt (stream-car input-stream))))

(define (smooth s)
  (let ((current stream-car s)
        (next (stream-car (stream-cdr s))))
    (cons-stream (/ (+ current next) 2)
                 (smooth (stream-cdr s)))))
