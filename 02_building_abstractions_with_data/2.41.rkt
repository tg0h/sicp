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
  (accumulate append nil
              (map (lambda (i)
                     (map (lambda (j)
                            (list i j))
                          (enumerate-interval 1 (- i 1))
                          ))
                   (enumerate-interval 1 n)
                   ))

  )

(define (test n)
  (map (lambda (i)
         (map (lambda (j)
                (map (lambda (k)
                       (list i j k))
                     (enumerate-interval 1 n))
                )
              (enumerate-interval 1 n)
              )
         )
       (enumerate-interval 1 n))
  )

(test 3)

