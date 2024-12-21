#lang sicp

(eq? 'a 'a)
(eq? 1 2)

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    (else #f)
    )
  )

(define (equal? a b)
  (cond
    ((and (not (pair? a)) (not (pair? b))) (eq? a b))
    ((and (pair? a) (pair? b)) ((and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))
    (else #f)

    )
  )

(define (_equal? a b)
  (cond (
         ((and (pair? a)) (eq? a b))
         ((and (pair? a)) (eq? a b))
         (else a)
         )
        )
  )

