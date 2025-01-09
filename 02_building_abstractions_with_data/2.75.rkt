#lang sicp


(define (square x ) (* x x))
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)


(define (apply-generic op . args)
  ; use map type-tag because we want to be generic, we want to provide many contents and many types for the contents
  ; for now only one arg eg real-part z not real-part z1 z2
  (let ((type-tags (map type-tag args))) ; get the type-tag ('rectangular) from the args eg ('rectangular ( 1 . 1 ) )
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args)) ; get the contents ( ( 1 . 1 ) ) from the args eg ('rectangular ( 1 . 1 ) )
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(define (apply-generic op arg) (arg op))

(define z1 (make-from-real-imag 1 1))
z1
(z1 'real-part)
(z1 'imag-part)
(z1 'magnitude)

