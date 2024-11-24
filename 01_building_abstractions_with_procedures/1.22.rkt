#lang sicp

(define (square x) (* x x))
(define (smallest-divisor n) (find-divisor n 2))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (even? n)
  (= (remainder n 2) 0))

(define (search-for-primes range)
  (define (search-for-3-primes candidate n)
    (cond
      ((= n 3) display "end")
      ((even? candidate) (search-for-3-primes (+ candidate 1) n))
      (else (if (timed-prime-test candidate)
                (search-for-3-primes (+ 2 candidate) (+ 1 n))
                (search-for-3-primes (+ 2 candidate) n)
                ))
      )
    )
  (search-for-3-primes range 0)
  )

(search-for-primes 1000)
