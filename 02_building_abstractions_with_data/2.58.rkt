#lang sicp

(define (sum? expr)
  ;; (eq? '+ (smallest-op expr))
  (if (memq '+ expr) true false)
  )

;; (sum? '(a + b * c))

(define (product? expr)
  ;; (eq? '* (smallest-op expr))
  (if (memq '* expr) true false)
  )

;; (sum? '(+ 1 2))

(define (singleton? l) (if (null? (cdr l)) true false))

(define (augend expr)
  (let ((a (cdr (memq '+ expr))))
    (if (singleton? a)
        (car a)
        a)))


; get everything before the + (a kind of reverse memq)
(define (addend expr)
  (define (iter result sub-list)
    (cond
      ((eq? (car sub-list) '+) result)
      ((null? sub-list) result)
      (else (iter (append result (list (car sub-list))) (cdr sub-list)))
      )
    )
  (let
      ( (iter-result (iter '() expr)))
    (if (singleton? iter-result) (car iter-result) iter-result)
    )
  )

(define test-sum '( d * b + ( a + b ) + 2 +  (c + d ) ))
(define test-sum2 '( d  + ( a + b ) + 2 +  (c + d ) ))
;; (addend test-sum)
;; (addend test-sum2)


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;; (define (make-sum a1 a2) (list '+ a1 a2))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;; (define (make-product m1 m2) (list '* m1 m2))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;; (define (sum? x) (and (pair? x) (eq? (car x) '+)))
;; (define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
;; (define (addend s) (cadr s))
;; (define (addend s) (car s))

;; (addend test-sum)

;; (define (augend s) (caddr s))
;; (define (augend s) (simplify (cddr s)))
;; (define (multiplicand p) (simplify (cddr p)))

;; (define (product? x) (and (pair? x) (eq? (car x) '*)))
;; (define (product? x) (and (pair? x) (eq? (cadr x) '*)))
;; (define (multiplier p) (cadr p))
;; (define (multiplier p) (car p))
;; (define (multiplicand p) (caddr p))

; get everything after the *
(define (multiplicand p)
  (let ((a (cdr (memq '* p))))
    (if (singleton? a)
        (car a)
        a)))

; get everything before the * (a kind of reverse memq)
(define (multiplier expr)
  (define (iter result sub-list)
    (cond
      ((eq? (car sub-list) '*) result)
      ((null? sub-list) result)
      (else (iter (append result (list (car sub-list))) (cdr sub-list)))
      )
    )
  (let
      ( (iter-result (iter '() expr)))
    (if (singleton? iter-result) (car iter-result) iter-result)
    )
  )

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


(define test-prod '( d * b + ( a + b ) + 2 +  (c + d ) ))
(define test-prod2 '( d  + ( a * b ) + 2 * (c + d ) ))
;; (multiplier test-prod2)
;; (multiplicand test-prod2)

;; (deriv '(1 + 2 * x) 'x)
;; (deriv '(x + 3 * (x * y * x) ) 'x)
(deriv '(x * y * x) 'x)

;; (multiplicand '(x * y ))
;; (multiplier '(x * y ))
