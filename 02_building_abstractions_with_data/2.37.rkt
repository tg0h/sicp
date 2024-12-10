#lang sicp

(define v list (1 2))

(define w 
  list (
    list (1 2)
    list (3 4)
  )
  )

(map * v w)
