#lang sicp

(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse( cdr l)) (list (car l)))
      )
  )

;; (reverse (list 1 2 3))

(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))
      x))


(define x (list 1 (list 2 3)))
x
(cdr x)
(car x)
;; y
;; (car y)
;; (cdr y)

(define z (list (list 2 3)))
z
(car z)

(append '() (list 1 2))
;; (deep-reverse (cdr x ))
;; (append(cdr x) (list 1 2))
;; (define y (list (list 1 2) (list 3 4)))
;; y
;; (deep-reverse x )
;; (deep-reverse y )

(define zz (list 3))

(append (cdr zz) (list (car zz)))
