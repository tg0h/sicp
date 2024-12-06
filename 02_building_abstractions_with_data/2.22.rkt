#lang sicp

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items))))
  )

(define (_square-list items) (map (lambda (x) (* x x )) items))

(square-list (list 1 2 3 4))
(_square-list (list 1 2 3 4))

(define (square x )(* x x ))

(define (ssquare-list items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(ssquare-list (list 1 2 3 4))
