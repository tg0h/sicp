#lang sicp

;; (reverse (list 1 4 9 16 25))

(define (pop l)
  (if (null? (cdr l))
      nil
      (cons (car l) (pop (cdr l)))
      )
  )
(pop (list 1 4 9 16 25))

;; (define (reverse l)
;;   if (null? l)
;;   )
