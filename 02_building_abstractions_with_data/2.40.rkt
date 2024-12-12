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

(unique-pairs 2)


;; prime
(define (next test-divisor)
  (if (= test-divisor 2) 3 (+ test-divisor 2)) ; more efficient to add 2 to search for next prime
  )

(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ;; (else (find-divisor n (+ test-divisor 1)))))
        (else (find-divisor n (next test-divisor ))))) ; more efficient

(define (prime? n)
  (= n (smallest-divisor n)))



(define (prime-sum? pair) (prime? (+ (car pair) (cadr pair))))
(define (flatmap proc seq) (accumulate append nil (map proc seq)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))
