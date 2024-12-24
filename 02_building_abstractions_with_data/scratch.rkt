#!/usr/bin/env racket
#lang sicp


(define *precedence-table*
  '( (maxop . 10000)
     (minop . -10000)
     (+ . 0)
     (* . 1) ))

(define (operator? x)
  (define (loop op-pair)
    (cond ((null? op-pair) #f)
          ((eq? x (caar op-pair)) #t)
          (else (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (min-precedence a b)
  (if (precedence<? a b)
      a
      b))

(define (precedence<? a b)
  (< (precedence a) (precedence b)))

(define (precedence op)
  (define (loop op-pair)
    (cond ((null? op-pair)
           (error "Operator not defined -- PRECEDENCE:" op))
          ((eq? op (caar op-pair))
           (cdar op-pair))
          (else
           (loop (cdr op-pair)))))
  (loop *precedence-table*))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (smallest-op expr)
  (accumulate (lambda (a b)
                (if (operator? b)
                    (min-precedence a b)
                    a))
              'maxop
              expr))

(define (sum? expr)
  ;; (eq? '+ (smallest-op expr))
  (if (memq '+ expr) true false)
  )

(sum? '(a + b * c))
(sum? '(a + ( b * c ) ))
