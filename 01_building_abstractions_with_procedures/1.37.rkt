#lang sicp

;; (define (cont-frac n d k )
;;   (define (iter n d k i result)
;;     (cond
;;       ((> i k ) result)
;;       (else (/ (n i) (d i)) (iter ) )
;;       )
;;
;;     )
;;   (iter n d 3 1 1)
;;   )

(define (cont-frac n d k)
  (if (= k 1) 1
      (/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))
      )
  )

;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 3)
;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 4)
;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)
;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
;; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 15)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 20)
