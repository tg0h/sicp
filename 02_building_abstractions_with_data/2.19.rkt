#lang sicp

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

(define us-coins (list 50 25 10 5 1))
(define _us-coins (list 25 1 50 10 5 ))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (_cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (_cc amount
                 (except-first-denomination
                  coin-values))
            (_cc (- amount
                    (_first-denomination
                     coin-values))
                 coin-values)))))



(define (_first-denomination l) (car l))
(define (except-first-denomination l) (cdr l))
(define (no-more? l) (= (length l) 0))

(_cc 100 us-coins)
(_cc 100 _us-coins)
