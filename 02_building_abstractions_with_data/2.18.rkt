#lang sicp

(reverse (list 1 4 9 16 25))

(define (pop l)
  (if (null? (cdr l))
      (car nil)
      (cons (car l) (pop (cdr l)))
      )
  )

;; (define (reverse l)
;;   if (null? l)
;;   )
