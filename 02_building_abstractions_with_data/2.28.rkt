#lang sicp

(define (fringe x)
  (define (iter result)
    (cond
      ((null? x) nil)
      ((not(pair? x) nil) x)
      (else
       (append (fringe (car x)) (fringe (cdr x)))
       )
      )
    )
  (iter (list nil))
  )

(define x (list (list 1 2) (list 3 4)))

(fringe x)
