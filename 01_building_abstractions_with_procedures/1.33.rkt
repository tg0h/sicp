#lang sicp

(define (identity x) x)
(define (next x) (+ x 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result)))
    )
  (iter a 0)
  )

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result)))
    )
  (iter a 1)
  )

;; (sum identity 1 next 4)
;; (product identity 1 next 4)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result)))
    )
  (iter a null-value)
  )

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond
      ((> a b) result)
      ((filter a) (iter (next a) (combiner (term a) result)))
      (else (iter (next a) result)))
    )
  (iter a null-value)
  )

