#lang sicp

(define counted '())

(define (count-pairs x)
  (define (search x counted)
    (cond ((null? counted) false)
          ((eq? x (car counted)) true)
          (else (search x (cdr counted))))
    )
  (define (count x counted)
    (define counted-before? (search x counted))
    ;; (display counted-before?)
    (if counted-before?
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
  (count x counted)
  )

(define l3 '( a b c))
(count-pairs l3)

;; (define (search x counted)
;;   (cond ((null? counted) false)
;;         ((eq? x (car counted)) true)
;;         (else (search x (cdr counted))))
;;   )
;; (search l3 counted)
