#lang sicp

(define (for-each f l)
  (if (not (null? l))
      (f (car l))
      (for-each f (cdr l))
      )
  )

(for-each
 (lambda (x) (newline)
   (display x))
 (list 57 321 88))
