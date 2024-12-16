#lang sicp
(#%require sicp-pict)
(paint einstein)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (right-split einstein 3))

;; (paint (beside einstein einstein))
;; (paint
;;  (beside (beside einstein einstein)
;;          (beside einstein einstein))
;;  )

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split einstein 1))
