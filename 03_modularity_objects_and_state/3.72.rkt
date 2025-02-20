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


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define pii (pairs integers integers))

(define (stream-for-each s n limit)
  (if (or (stream-null? s ) (= n limit ))
      'done
      (begin  (display n)
              (display ": ")
              (display (stream-car s))
              (newline)
              (stream-for-each (stream-cdr s) (+ n 1 ) limit))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (square x) (* x x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((w1 (weight s1car))
                 (w2 (weight s2car)))
             (cond ((< w1 w2)
                    (cons-stream s1car
                                 (merge-weighted (stream-cdr s1) s2 weight)))
                   ((> w1 w2)
                    (cons-stream s2car
                                 (merge-weighted s1 (stream-cdr s2) weight)))
                   (else
                    (cons-stream
                     s1car
                     (cons-stream
                      s2car ;; must include both in case of ties!
                      (merge-weighted
                       (stream-cdr s1)
                       (stream-cdr s2)
                       weight))))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))


(define (cube x) (* x x x))
(define (sum-weight p) (+ (car p) (cadr p)))
(define (cube-weight p) (+ (cube (car p)) (cube (cadr p))))
(define (square-weight p) (+ (square (car p)) (square (cadr p))))

(define wii (weighted-pairs integers integers square-weight))

;; (stream-for-each wii 1 20)

(define (square-nums-three-ways s)
  (let ((current (stream-car s))
        (next (stream-car (stream-cdr s)))
        (next-next (stream-car (stream-cdr (stream-cdr s)))))
    (if (= (square-weight current) (square-weight next) (square-weight next-next))
        (cons-stream (list (square-weight current) current next next-next)
                     (square-nums-three-ways (stream-cdr s)))
        (square-nums-three-ways (stream-cdr s))
        )
    ))

(define sntw (square-nums-three-ways wii))

;; (stream-ref sntw 0)
;; (stream-ref sntw 1)
;; (stream-ref sntw 2)
;; (stream-ref sntw 3)
(stream-for-each sntw 1 20)
