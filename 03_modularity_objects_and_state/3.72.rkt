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

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))




;; (stream-for-each ti 1 70)


(define (square x) (* x x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
           (cond ((< s1car s2car) (cons-stream
                                   s1car
                                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else (cons-stream
                        s1car
                        (merge (stream-cdr s1)
                               (stream-cdr s2)))))))))


;; (stream-for-each pii 1 20)

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

;; (define (weighted-pairs s t weight)
;;   (cons-stream
;;    (list (stream-car s) (stream-car t))
;;    (merge-weighted
;;     (merge-weighted
;;      (stream-map (lambda (x) (list x (stream-car t)))
;;                  (stream-cdr s))
;;      (stream-map (lambda (x) (list (stream-car s) x))
;;                  (stream-cdr t))
;;      weight)
;;     (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
;;     weight)))

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

(define wii (weighted-pairs integers integers cube-weight))

(stream-for-each wii 1 20)

(define (find-rama-nums)
  (define (search s)
    (let ((current (stream-car s))
          (next (stream-car (stream-cdr s))))
      (if (= (cube-weight current) (cube-weight next))
          (begin
            (display current)
            (display " and ")
            (display next)
            (display " : ")
            (display (cube-weight current))
            (newline)
            (search (stream-cdr (stream-cdr s)))
            )
          (search (stream-cdr s))
          )))
  (search wii)
  )

(find-rama-nums)
