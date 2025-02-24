#lang sicp

(define (operation exp) (car exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) ;; primitive - string or number
         exp)

        ((variable? exp) ;; is exp a symbol? (not a list starting with the symbol quote)
         (lookup-variable-value exp env))

        (else
         ((get 'eval (operation exp)) exp env)
         )

        ((quoted? exp) ;; quoted - a list starting with the symbol quote
         (text-of-quotation exp))
        ;; change text-of-quotation to accept env
        ;; put 'eval 'quote text-of-quotation

        ((assignment? exp) ;; assignment - set! x 2
         (eval-assignment exp env))
        ;; put 'eval 'set! eval-assignment

        ((definition? exp) ;; definition - add the variable to the environment, the value can be simple or a lambda
         (eval-definition exp env))
        ;; put 'eval 'define eval-definition

        ((if? exp) ;; if
         (eval-if exp env))
        ;; put 'eval 'define eval-if

        ((lambda? exp) ;; lambda
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ;; convert the lambda function to a function that accepts exp and env
        ;; put 'eval 'lambda lambda-operation
        ;; (define (lambda-operation exp env)
        ;;   (make-procedure (lambda-parameters exp)
        ;;                   (lambda-body exp)
        ;;                   env)
        ;;   )

        ((begin? exp) ;; begin
         (eval-sequence
          (begin-actions exp) env))
        ;; put 'eval 'begin begin-operation
        ;; convert the begin function to a function that accepts exp and env
        ;; (define (begin-operation exp env)
        ;; (eval-sequence
        ;;  (begin-actions exp) env)
        ;;   )

        ((cond? exp) ;; cond
         (eval
          (cond->if exp) env))
        ;; put 'eval 'cond cond-operation
        ;; convert the cond function to a function that accepts exp and env
        ;; (define (cond-operation exp env)
        ;; (eval
        ;;  (cond->if exp) env)
        ;;   )

        ((application? exp) ;; function call
         (meta-apply
          (eval (operator exp) env)
          (list-of-values (operands exp) env)))

        (else
         (error "Unknown expression type: EVAL" exp))))
