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
  (/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))
  )
