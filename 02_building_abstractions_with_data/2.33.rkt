#lang sicp

(define (accumulate op initial sequence) (if (null? sequence)
                                             initial
                                             (op (car sequence)
                                                 (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence)
  )

(define square (lambda (x) (* x x)) )

(map square (list 1 2 3))
