#lang sicp

(define (square m)
  (display "square ")(display m)(newline)
  (* m m))
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
  ;; (if (prime? n)
  (if (fast-prime? n 100) ; use fermat test
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
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))
  )

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)) ; if true, by fermats little theorem, n is likely prime, (must be true for ALL a < n for n to be prime)
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(expmod 5 101 101)

; Helper procedures
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
;; (define (square x) (* x x))
(define (report-elapsed-time start-time)
  (display " *** ")
  (display (- (runtime) start-time)))

; The original & modified procedures
;; (define (expmod base exp m)
;;   (cond ((= exp 0) 1)
;;         ((even? exp)
;;          (remainder
;;           (square (expmod base (/ exp 2) m)) ; (1)
;;           m))
;;         (else
;;          (remainder
;;           (* base (expmod base (- exp 1) m))
;;           m))))
;;
(define (modified-expmod base exp m)
  (remainder (fast-expt base exp) m))

; Test the speed
(define start-time (runtime))
(expmod 999999 1000000 1000000)
(report-elapsed-time start-time)

(modified-expmod 999999 1000000 1000000)
(report-elapsed-time start-time)