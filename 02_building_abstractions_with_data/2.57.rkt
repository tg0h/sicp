#lang sicp

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;; (define (make-sum a1 a2) (list '+ a1 a2))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;; (define (make-product m1 m2) (list '* m1 m2))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
  (let
      ((rest (caddr s)))
    (if (not (pair? rest)) (make-sum (car s) (cdr s) ))
    )
  )

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))


(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (make-exponent b e)
  (cond ((=number? e 0) 1) ; something power 0 is 1
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (power b e))
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))

        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponent (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var))
         )
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(+ (+ y 1) v ) 'x)

(define test '(+ x 1))
(cadr test)
(caddr test)
(cddr test)
(define _test (cddr test))
(cdr _test)

;; (augend '(+ x 1 a))
