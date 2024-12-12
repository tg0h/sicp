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

(define (ordered-triple? triple)
  (let
      (
       (i (car triple))
       (j (cadr triple))
       (k (caddr triple))
       )
    (and
     (<= i j)
     (<= j k)
     )
    )
  )

(define (triple-sums-to-n? n)
  (lambda(triple)
    (let
        (
         (i (car triple))
         (j (cadr triple))
         (k (caddr triple))
         )
      (= (+ i j k) n)
      )
    )
  )

(define (generate-trips n s)
  (filter (triple-sums-to-n? s)
          (filter ordered-triple?
                  (generate-triple n)
                  )
          )
  )

;; (generate-trips 4 10)


(filter ordered-triple?
        (generate-triple 2)
        )

(define (ordered-triples-sum n s)
  (filter (lambda (list) (= (accumulate + 0 list) s))
          (flatmap
           (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n))))


;; (define trip (list 1 2 3))
;; (car trip)
;; (cadr trip)
;; (caddr trip)
