#lang sicp

(define (square x) (* x x))
;; (square 21)

;; (define (abs x)
;;   (cond 
;;       ((> x 0) x)
;;       ((= x 0) 0)
;;       ((< x 0) (- x))
;;   )
;; )
;;
;; abs -3

;; 10
;; (+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1)) 
;; (= a b)
;;
;; (if (and (> b a) (< b (* a b) ))
;;   b
;;   a
;;   )


;; (cond ((= a 4) 6)
;; ((= b 4) (+ 6 7 a))
;; (else 25))

;; (+ 2 (if (> b a) b a))

;; (* (cond ((> a b) a) ((< a b) b)
;; (else -1)) (+ a 1))

;; (+ 5 4 (- 2 (- 3 (+ 6 0.8))))
;; (/ (+ 5 4(- 2 (- 3 (+ 6 0.8)))) (* 3 (- 6 2) (- 2 7)))
;;
;; (* 3 (- 6 2) (- 2 7))

;; (define larger_two a b c)

;; (<= 4 3)
;; (define (square x) (* x x))

;; ex 1.3
;; (define (<= a b) (or (< a b) (= a b)))
;; (define (larger_two_squares a b c)
;;   (cond 
;;       ((and (<= a b) (<= a c)) (+ (square b) (square c)))
;;       ((and (<= b a) (<= b c)) (+ (square a) (square c)))
;;       ((and (<= c a) (<= c b)) (+ (square a) (square b)))
;;   )
;; )
;; (larger_two_squares 1 1.1 3)


; ex1.4
;; (define (a-plus-abs-b a b) ((if (> b 0) + -) a b))

;; (a-plus-abs-b -1 3)


(let ((x 3)
(y (+ x 2)))
(* x y))
