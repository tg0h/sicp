#lang sicp

;; (reverse (list 1 4 9 16 25))

(define (pop l)
  (if (null? (cdr l))
      nil
      (cons (car l) (pop (cdr l)))
      )
  )
(pop (list 1 4 9 16 25))

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))
      )
  )

(define (reverse l)
  (if (null? (cdr l))
      l
      (cons (last-pair l) (reverse (pop  l)))
      )
  )

(reverse (list 1 4 9 16 25))

(cons 3 nil)
