#!/usr/bin/env racket
#lang sicp

(define (=number? exp num) (and (number? exp) (= exp num)))
(define (_make-sum a1 . rest)
  (display "a1 is->") (display a1) (display " rest is->") (display rest)
  (newline)
  (display "car rest is->") (display (car rest))
  (newline)
  (display "cdr rest is->") (display (cdr rest))
  (newline)
  (display "null rest is->") (display (null? rest))
  (newline)
  (display "dnull rest is->") (display (null? (cdr rest)))
  (newline)
  (newline)
  (let
      (
       (rest_length (length rest))
       (rest_value (car rest))
       )
    (cond
      ((null? (car rest)) a1)
      ;; ((=number? a1 0) rest_value)
      ;; ((=number? a2 0) a1)
      ;; ((and (number? a1) (number? a2)) (+ a1 a2))
      (else
       ;; a1
       (list '+ a1 (_make-sum (car rest) (cdr rest)))
       ;; (else (list '+ a1 ))
       )
      )
    )
  )

;; (_make-sum 1 2)

;; (_make-sum 1 nil)
(_make-sum 1 2 3 )


;; (list '+ 1 (list 2 3))
