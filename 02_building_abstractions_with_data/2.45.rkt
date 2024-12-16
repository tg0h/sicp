#lang sicp
(#%require sicp-pict)
(paint einstein)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))



(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; (paint (up-split einstein 1))

(define (split op1 op2)
  (lambda(painter n)
    (define (iter n)
      (if (= n 0) painter
          ( let ((smaller (iter (- n 1))))
             (op1 painter (op2 smaller smaller))
             )
          )
      )
    (iter n)
    )
  )

(define _right-split (split beside below))

(paint (right-split einstein 3))
(paint (_right-split einstein 3))
