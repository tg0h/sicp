#lang sicp

(define (sum term a next b)
  ;; (display "term ")
  ;; (display a)
  ;; (display b)
  ;; (display "=")
  ;; (display (term a))
  ;; (newline)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
