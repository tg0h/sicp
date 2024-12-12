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

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))) (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq) (accumulate append nil (map proc seq)))

(define (generate-triple n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 n))
                        )
                      (enumerate-interval 1 n)
                      )
             )
           (enumerate-interval 1 n))
  )

(generate-triple 2)

;; (filter (lambda (triple)
;;           (and (car triple))
;;           ) generate-triple 2
;;         )


(define trip (list 1 2 3))
(car trip)
(cadr trip)
(caddr trip)
