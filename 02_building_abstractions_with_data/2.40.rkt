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

(unique-pairs 3)


;; prime?
(define (next test-divisor) (if (= test-divisor 2) 3 (+ test-divisor 2)) )
(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor )))))
(define (prime? n)
  (= n (smallest-divisor n)))


(define (prime-sum? pair) (prime? (+ (car pair) (cadr pair))))
(define (flatmap proc seq) (accumulate append nil (map proc seq)))

(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))) (else (filter predicate (cdr sequence)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               ;; (flatmap (lambda (i)
               ;;            (map (lambda (j) (list i j))
               ;;                 (enumerate-interval 1 (- i 1))))
               ;;          (enumerate-interval 1 n))
               (unique-pairs n)
               )))

(prime-sum-pairs 10)
