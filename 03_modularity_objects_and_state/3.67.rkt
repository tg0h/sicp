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
  (if (or (stream-null? s )
          (= n limit )
          ;; (= (stream-car (stream-car s)) 99)
          )
      'done
      (begin  (display n)
              (display ": ")
              (display (stream-car s))
              (newline)
              (stream-for-each (stream-cdr s) (+ n 1 ) limit))))
;; (define (display-line x) (display x)(newline))

;; (define (display-stream s) (stream-for-each display-line s 0))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; (stream-for-each pii 1 40)

(define (all-ints s)
  (let (
        (first (car (stream-car s )))
        (second (cadr(stream-car s ))))
    (cond
      ((= first second) (cons-stream (list first second) (all-ints (stream-cdr s))))
      (else (cons-stream (list first second)
                         (cons-stream (list second first)
                                      (all-ints (stream-cdr s))))))))

(define apii (all-ints pii))

(stream-for-each apii 1 30)
