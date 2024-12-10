#lang sicp


(define  square
  ;; (* x x)
  (lambda (x)(* x x ))
  )

(define (tree-map f t)
  (map (lambda (x)
         (cond ((pair? x) (tree-map f x))
               (else (f x))
               )
         ) t
           )
  )

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))



;; (square 2)
