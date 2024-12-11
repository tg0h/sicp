#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define v (list 1 2))
(define w (list 3 4))

(define (dot-product v w)
  (accumulate + 0 (map * v w))
  )

;; (dot-product v w)


(define (matrix-*-vector m v)
  (map (lambda (matrix-row) (dot-product matrix-row v)) m)
  )

(define m 
  (list 
  (list 1 2)
  (list 3 4)
  )
  )

(matrix-*-vector m v)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))
      )
  )
(define (transpose mat)

  )
