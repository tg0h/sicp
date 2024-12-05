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
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


(define (make-center-percent c p)
  (make-interval (- c (* p c)) (+ c (* p c))))


(define (percent i)
  (/ (width i) (center i))
  )

(define int (make-interval 1 2))

(define int2 (make-center-width 1.5 0.5))

;; (width int)
;; (center int)
;; (percent int)
;; (define a (make-center-percent 1 0.9))
;; (define b (make-center-percent 2 0.4))
;; (define c (mul-interval a b))
;; (percent c)

(define (contains-zero x) (and (< (lower-bound x) 0) (> (upper-bound x) 0)))
(define (div-interval x y)
  ( if (contains-zero y) (error "division by zero")
       (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))))
       )
  )

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))



(define a (make-center-percent 1 0.01))
(define b (make-center-percent 2 0.01))
(define c (make-interval 2 8))

;; (par1 a b)
;; (par2 a b)
;; a
;; c
;; (div-interval c c)
(div-interval a b)
(div-interval a a)
