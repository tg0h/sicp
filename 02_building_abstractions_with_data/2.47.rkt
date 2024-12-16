#lang sicp

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (_make-frame origin edge1 edge2) 
  (cons origin (cons edge1 edge2)))
