#lang sicp

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile)
  (car mobile)
  )

