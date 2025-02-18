#lang sicp

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s) the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (newline)
  (display "SHOW x-> ")
  (display x)
  (newline)
  )

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (show x)
  (display-line x) x
  )

;-----------------------
(define y (stream-enumerate-interval 0 10))
;; (stream-cdr y)
;; (stream-cdr y)
;; (stream-car (stream-cdr (stream-cdr y)))
;; (stream-cdr y)

(define x
  (stream-map show (stream-enumerate-interval 0 10))
  )

;; (stream-cdr x)
;; (newline)
(stream-cdr (stream-cdr x)) ; cached
(stream-cdr (stream-cdr x)) ; cached

; this is a new cons-stream so it is not cached because it has never been called before
(stream-cdr (stream-cdr   (stream-map show (stream-enumerate-interval 0 10)))) ; not cached
;; (stream-cdr   (stream-map show (stream-enumerate-interval 0 10)))

;; x
;; x
;; (stream-cdr x)
;; (stream-ref x 5)

;; (stream-ref x 7) ; (memoized)
