#lang sicp

(define (enumerate-interval low high)
  (if (> low high) nil (cons low (enumerate-interval (+ low 1) high)))
  )
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (unique-pairs n)
  (accumulate
   append
   nil
   (map (lambda (i)
          (map (lambda (j)
                 (list i j))
               (enumerate-interval 1 (- i 1))
               ))
        (enumerate-interval 1 n)
        )
   )
  )

(unique-pairs 2)
