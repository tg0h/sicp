#lang sicp

(define (double f) (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1)
  )

;; (inc (inc 1) )
((double inc) 1) ; adds 2 to a
((lambda (x) (inc (inc x))) 1)
((double double) 1) ; adds 2 to a

((lambda (x) (double (double x))) 1)


;; (((double double) inc) 5)
(((double (double double)) inc) 5)
;; ((double (double (double inc))) 5)

(((lambda (x) ((double double) ((double double) x))) inc) 5)

(((lambda (x) ((lambda (x) (double (double x)))
               ((lambda (x) (double (double x))) x))) inc) 5)

(((lambda (x) ((lambda (x) (double (double x)))
               (double (double x)))) inc) 5)

(((lambda (x) (double (double (double (double x))))) inc) 5)

((double (double (double (double inc)))) 5)

;; https://github.com/mngu2382/sicp/blob/master/fragments/Ex1-41.scm
