#lang sicp
(#%require sicp-pict)
(paint einstein)

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(paint (flip-vert einstein))
