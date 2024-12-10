#lang sicp

(define (accumulate op initial sequence) (if (null? sequence)
                                             initial
                                             (op (car sequence)
                                                 (accumulate op initial (cdr sequence)))))

(accumulate cons nil (list 1 2 3))

(define (enumerate-tree tree)
  (cond
    ((null? tree) nil)
    ((not(pair? tree)) (list tree))
    (else (append
           (enumerate-tree (car tree))
           (enumerate-tree (cdr tree))
           )))
  )

(enumerate-tree (list 1 (list 2 3)))



(define identity
  (lambda (x) (* 2 x))
  )

(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (map (lambda (x) x) (enumerate-tree t))
              )
  )

(count-leaves (list 1 (list 2 3 4 5)))
