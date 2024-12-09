#lang sicp

(define (square-tree x)
  (cond
    ((null? x) x)
    ((not (pair? x)) (* x x ))
    (else
     (cons (square-tree (car x)) (square-tree (cdr x))
           )
     )
    )
  )


(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (_square-tree x)
  (map (lambda (x)
         (cond ((pair? x) (_square-tree x))
               (else (* x x))
               )
         ) x
           )
  )



(_square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
