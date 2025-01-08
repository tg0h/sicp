#lang sicp

; section 3.3.3
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false)) false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value) (set-cdr! subtable
                                                    (cons (cons key-2 value)
                                                          (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

; deriv
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
(define (augend s) (caddr s))
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

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp) (make-sum (deriv (addend exp) var)
;;                               (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;
;;         ((exponentiation? exp)
;;          (make-product
;;           (make-product (exponent exp)
;;                         (make-exponent (base exp) (- (exponent exp) 1)))
;;           (deriv (base exp) var))
;;          )
;;         (else
;;          (error "unknown expression type: DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else (
               (get 'deriv (operator exp))
               ;; (get (operator exp) 'deriv) ; what if you switch the position ?
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-sum )
  (define (deriv-sum operands var)
    (let
        ((op1 (car operands))
         (op2 (cadr operands)))
      (make-sum (deriv op1 var) (deriv op2 var))
      )
    )
  (put 'deriv '+ deriv-sum)
  )

(define (install-deriv-product)
  (define (deriv-product operands var)
    (let
        ((op1 (car operands))
         (op2 (cadr operands)))
      (make-sum
       (make-product op1 (deriv op2 var))
       (make-product op2 (deriv op1 var))
       )
      )
    )
  (put 'deriv '* deriv-product)
  )

(define (install-deriv-exponentiation)
  (define (deriv-exponentiation operands var)
    (let
        ((op1 (car operands))
         (op2 (cadr operands)))
      (make-product
       (make-product op2
                     (make-exponent op1 (- op2 1)))
       (deriv op1 var))
      )
    )
  (put 'deriv '** deriv-exponentiation)
  )

(install-deriv-sum)
(install-deriv-product)
(install-deriv-exponentiation)



;; '(** u n)
;; (base '(** u n))
;; (exponent '(** u n))
(deriv '(+ x 3) 'x)

(deriv '(+ x y) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

;; (define test '(** x 5))

;; (exponentiation? test)
;; (exponent test)
;; (base test)
;; (make-exponent (base test) (- (exponent test) 1))

(deriv '(** x 5) 'x)
(deriv (deriv '(** x 5) 'x) 'x)
