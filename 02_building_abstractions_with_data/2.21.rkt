#lang sicp

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items))))
  )

(define (_square-list items)
  (map (lambda (x) (* x x )) items))

(square-list (list 1 2 3 4))
(_square-list (list 1 2 3 4))
