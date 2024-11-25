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

(define (search-for-primes lower upper)
  (define (iter n)
    (cond
      ((> n upper) )
      ((even? n) (iter (+ n 1)))
      (else
       (timed-prime-test n)
       (iter (+ n 2)))
      )
    )
  (iter lower)
  )

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m)) (else
               (remainder
                (* base (expmod base (- exp 1) m))
                m)))
  )

(search-for-primes 1000000000 1000000021) ;~103
; when load increased by factor of 10, perf increased by factor of sqrt(10) ~ 3
(search-for-primes 10000000000 10000000061) ;~283 increased by a factor of sqrt(10) ~ 3
(search-for-primes 100000000000 100000000061) ;~283 increased by a factor of sqrt(10) ~ 3

;; (search-for-primes 1000)
;; (timed-prime-test 7)
;; (display newline)

;; (if (timed-prime-test 4) "a" "b" )
