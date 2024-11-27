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

(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))
  (define (square x) (* x x))
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (= n (smallest-divisor n)))


(define (square x) (* x x))

(filtered-accumulate prime? + 0 square 1 next 7)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


