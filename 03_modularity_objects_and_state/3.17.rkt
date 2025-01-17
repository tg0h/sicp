#lang sicp

(define (count-pairs x)
  (define (search x counted)
    (cond ((null? counted) false)
          ((eq? x (car counted)) true)
          (else (search x (cdr counted))))
    )
  (define (count x counted)
    (define counted-before? (search x counted))
    (if (counted-before?)
        (begin
          (set! counted (cons x counted))
          (if (not (pair? x))
              0
              (+ (count (car x) counted)
                 (count (cdr x) counted))
              ))
        (if (not (pair? x))
            0
            (+ (count (car x) counted)
               (count (cdr x) counted)

               1)
            )
        )
    )
  (count x '())
  )
