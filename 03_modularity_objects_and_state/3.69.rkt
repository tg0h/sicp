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

;; (define (triples s t u)
;;   (cons-stream (stream-map (lambda (p) (list (stream-car s)
;;                                              (car p)
;;                                              (cadr p)))
;;                            (pairs t u))
;;                (triples
;;                 (stream-cdr s)
;;                 (stream-cdr t)
;;                 (stream-cdr u))))

(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
               (interleave
                (stream-map (lambda (p) (list (stream-car s)
                                              (car p)
                                              (cadr p)))
                            (stream-cdr (pairs t u)))
                (triples
                 (stream-cdr s)
                 (stream-cdr t)
                 (stream-cdr u)))))

(define ti (triples integers integers integers))


(stream-for-each ti 1 70)


(define (square x) (* x x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define pythagorean-triples
  (stream-filter (lambda(t)
                   (let ((i (car t))
                         (j (cadr t))
                         (k (caddr t)))
                     (= (+ (square i) (square j)) (square k)))) ti))

;; (stream-for-each pythagorean-triples 1 10)

;; (stream-for-each (pairs integers (stream-cdr integers)) 1 20)
