#lang sicp

(define (has-cycle? l)
  (define seen-before '())
  (if (memq (car l) seen-before) true
      ( begin
         (set! seen-before (cons (car l) seen-before))
         (has-cycle? (cdr l))
         )
      )
  )
