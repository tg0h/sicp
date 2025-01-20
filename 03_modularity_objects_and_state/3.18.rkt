#lang sicp

(define seen-before '())

(define (has-cycle? l)
  (if (memq (car l) seen-before) true
      ( begin
         (set! seen-before (cons (car l) seen-before))
         (has-cycle? (cdr l))
         )
      )
  )



(define (make-cycle x)
  (set-cdr! (last-pair x) x) x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

;; z

(has-cycle? z)
