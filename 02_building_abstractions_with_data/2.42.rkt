#lang sicp


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))) (else (filter predicate (cdr sequence)))))

(define (is-point-safe? point-a point-b)
  (define (horizontal? point-a point-b) (= (cadr point-a) (cadr point-b)))
  (define (diagonal? point-a point-b)
    (define (abs x) (cond ((> x 0) x) ((= x 0) 0) ((< x 0) (- x))))
    (let
        (
         (x-point-a (car point-a))
         (y-point-a (cadr point-a))
         (x-point-b (car point-b))
         (y-point-b (cadr point-b))
         )
      (= (abs (- y-point-b y-point-a)) (abs(- x-point-b x-point-a)))
      )
    )
  (and
   (not (horizontal? point-a point-b))
   (not (diagonal? point-a point-b))
   )
  )

(define (check-point-safe-with-positions test-point positions)
  (accumulate
   (lambda (point is-rest-of-points-safe)
     (and (is-point-safe? point test-point) is-rest-of-points-safe)
     )
   #t positions)
  )

(define (safe? k positions)
  (filter (lambda (position)
            (let
                (
                 (col-k-point (car (filter (lambda (point)
                                             (= (car point) k)
                                             ) position
                                               )))
                 (test-points (filter (lambda (point)
                                        (not(= (car point) k))
                                        ) position
                                          ))
                 )
              ;; (display col-k-point) (display "test against->") (display test-points)
              ;; (newline)
              (check-point-safe-with-positions col-k-point test-points)
              )
            ) positions
              )
  )
