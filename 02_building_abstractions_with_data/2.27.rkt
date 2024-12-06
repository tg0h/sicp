#lang sicp

;; (reverse (list 1 4 9 16 25))

(define (pop l)
  (if (null? (cdr l))
      nil
      (cons (car l) (pop (cdr l)))
      )
  )
;; (pop (list 1 4 9 16 25))

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

(define (deep-reverse l)
  (cond
    ((null? (cdr l)))
    ((pair? l) deep-reverse l)
    ((cons (last-pair l) (reverse (pop  l))))
    )
  )

;; (reverse (list 1 4 9 16 25))

(define x (list (list 1 2) (list 3 4)))
x
(last-pair x)
(last-pair(last-pair x))
