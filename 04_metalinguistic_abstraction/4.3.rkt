#lang sicp

(define (eval exp env)
  (cond ((self-evaluating? exp) ;; primitive - string or number
         exp)

        ((variable? exp) ;; is exp a symbol? (not a list starting with the symbol quote)
         (lookup-variable-value exp env))

        ((quoted? exp) ;; quoted - a list starting with the symbol quote
         (text-of-quotation exp))

        ((assignment? exp) ;; assignment - set! x 2
         (eval-assignment exp env))

        ((definition? exp) ;; definition - add the variable to the environment, the value can be simple or a lambda
         (eval-definition exp env))

        ((if? exp) ;; if
         (eval-if exp env))

        ((lambda? exp) ;; lambda
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))

        ((begin? exp) ;; begin
         (eval-sequence
          (begin-actions exp) env))

        ((cond? exp) ;; cond
         (eval
          (cond->if exp) env))

        ((application? exp) ;; function call
         (meta-apply
          (eval (operator exp) env)
          (list-of-values (operands exp) env)))

        (else
         (error "Unknown expression type: EVAL" exp))))
