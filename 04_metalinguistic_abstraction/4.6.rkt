#lang sicp


(define (eval exp env)
  (cond ((self-evaluating? exp) ;; primitive - string or number
         exp)
        ((variable? exp) ;; is exp a symbol? (not a list starting with the symbol quote)
         (lookup-variable-value exp env))

        ((quoted? exp) ;; togged-list - quoted - a list starting with the symbol quote
         (text-of-quotation exp))

        ((assignment? exp) ;; tagged-list - assignment - set! x 2
         (eval-assignment exp env))

        ((definition? exp) ;; tagged-list - definition - add the variable to the environment, the value can be simple or a lambda
         (eval-definition exp env))

        ((if? exp) ;; tagged-list - if
         (eval-if exp env))

        ((lambda? exp) ;; tagged-list - lambda
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ;; ((let? exp)
        ;;  (let->combination exp)

        ((begin? exp) ;; tagged-list - begin
         (eval-sequence
          (begin-actions exp) env))

        ((cond? exp) ;; tagged-list - cond
         (eval
          (cond->if exp) env))

        ((application? exp) ;; function call
         (meta-apply
          (eval (operator exp) env)
          (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))

(define (var-clause clause) (car clause))
(define (exp-clause clause) (cadr clause))

; create list of vars from let expression
(define (let-var-list let-clauses)
  (let ((first (car let-clauses)))
    (if (null? first)
        nil
        (cons (var-clause first) (let-var-list (cdr let-clauses)))
        )))

; create list of exprs from let expression
(define (let-exp-list let-clauses)
  (let ((first (car let-clauses)))
    (if (null? first)
        nil
        (cons (exp-clause first) (let-exp-list (cdr let-clauses)))
        )))

(define (let-body exp) (cddr exp))



(define (let->combination exp env)
  (make-procedure (let-var-list (let-clauses exp)) (let-body exp) env)

  )

;; (let
;;     (
;;      (a (+ 1 1))
;;      (b (+ 1 1))
;;      )
;;
;;   (+ 2 2)
;;   (+ 2 2)
;;   )
