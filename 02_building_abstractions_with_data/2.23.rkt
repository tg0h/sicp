#lang sicp

(define (_for-each f l)
  (if (not (null? l))
      (begin
        (display l)
        (f (car l))
        (newline)
        (_for-each f (cdr l))
        )
      (display "else")
      )
  )


;; (if (not(null? (list 7))) (newline) (display "tim"))

(_for-each
 (lambda (x)
   (display "printing:")
   (display x))
 (list 57 321 88))

