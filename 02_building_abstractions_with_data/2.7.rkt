#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (contains-zero x)
  (and
   (< (lower-bound x) 0)
   (> (upper-bound x) 0)))

(define (div-interval x y)
  ( if (contains-zero y) (error "division by zero")
       (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))))
       )
  )

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width x)
  (- (upper-bound x) (lower-bound x))
  )

(define a (make-interval 1 2))
(define b (make-interval 3 8))
(define c (make-interval -1 8))
( define (print-interval i)
   (display (lower-bound i))
   (display (upper-bound i))
   )

;; (add-interval a b)
(width a)
(width b)
;; (sub-interval a b)
;; (width (add-interval a b))
;; (width (sub-interval a b))

(mul-interval a b)
(width(mul-interval a b))
(div-interval a b)
(width(div-interval a b))
(contains-zero c)
(div-interval a c)


;; (print-interval a)
;; (lower-bound a )
