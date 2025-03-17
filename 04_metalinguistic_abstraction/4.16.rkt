#lang sicp

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-alternative exp) env)
      (eval (if-consequent exp) env)
      ))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp) ; does the variable exist? search all envs
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; numbers or strings
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true) (else false)))

; symbols
(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; assignment
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; quote
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; definition
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  ;; (define x 2) - simple
  ;; (define (square x) (* x x))
  (if (symbol? (cadr exp)) (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;formalparameters
                   (cddr exp)))) ;body

; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (let-clauses exp)))
(define (let-exps exp) (map cadr (let-clauses exp)))

(define (named-let? exp) (not (pair? (cadr exp))))
(define (named-let-procedure exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))
(define (named-let-vars exp) (map car (named-let-bindings exp)))
(define (named-let-exp exp) (map cadr (named-let-bindings exp)))
(define (named-let-transform-body exp)
  (cons
   ; define the procedure with the body
   ; does this pollute the global namespace?
   (cons 'define (cons
                  (cons (named-let-procedure exp) (named-let-vars exp))
                  (named-let-body exp)
                  ))
   ; call the procedure
   (list (cons (named-let-procedure exp) (named-let-exp exp)))
   )
  )

(define (let->combination exp)
  (if (named-let? exp)
      (cons (make-lambda (named-let-vars exp) (named-let-transform-body exp)) (named-let-exp exp))
      (cons (make-lambda (let-vars exp) (let-body exp)) (let-exps exp))
      ))

; let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cadr exp))
(define (let*-body exp) (cddr exp))
(define (let*->nested-lets exp) (expand-lets (let*-clauses exp) (let*-body exp)))

(define (expand-lets clauses body)
  (let ((first (car clauses))
        (rest (cdr clauses)))
    (if (null? rest)
        (begin
          ;; (display "making")
          ;; (display (cons (make-lambda (list (car first)) body) (cdr first)))
          ;; (newline)
          (cons (make-lambda (list (car first)) body) (cdr first))
          )
        (cons (make-lambda (list (car first)) (list (expand-lets (cdr clauses) body)))
              (cdr first))
        )))


; conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp)
      'false))

; if
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
; seq is a list of expressions ( (exp 1 ) (exp 2) )
; cons begin list gives a list of exps beginning with 'begin

; procedure application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond (convert cond to nested ifs)
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (cond-arrow-clause? clause) (eq? (cadr clause) '=>))
(define (cond-arrow-test clause) (car clause))
(define (cond-arrow-recipient clause) (caddr clause))

; do
(define (do? exp) (tagged-list? exp 'do))
(define (do-body exp) (cadr exp))
(define (do-pred exp) (caddr exp))

(define (make-define-procedure name variables body)
  (cons 'define (cons (cons name variables ) (list body) ))
  )
(define (eval-do exp)
  (sequence->exp
   (cons
    (do-body exp) ; do it once
    (cons ; then define the function if pred body recurse else nothing (false) then run r again
     (make-define-procedure 'r '() ; unfortunately, the name of the proc can shadow variables that the user defines
                            (make-if (do-pred exp)
                                     (sequence->exp (cons (do-body exp) '((r))))
                                     'false
                                     )
                            )

     '((r)) )
    )
   )
  )

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-arrow-clause? first)
            (make-if (cond-arrow-test first)
                     (list (cond-arrow-recipient first) (cond-arrow-test first))
                     (expand-clauses rest))
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last: COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

; test predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; represent procedures
(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned)
                 (error "variable is unassigned" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable lolz" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cadr cadr)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +) ; implement + COOOOOOL
        (list '- -)
        (list '= =)
        (list '* *)
        (list '< <)
        (list 'assoc assoc)
        ;; ⟨more primitives⟩
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (
   ;; apply-in-underlying-scheme
   ; use the underlying apply (note our implemented apply is called meta-apply
   apply (primitive-implementation proc) args))

(define (meta-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else (error
               "Unknown procedure type: meta-apply" procedure))))

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

        ; let to lambda
        ((let? exp) (eval
                     (let->combination exp) env))

        ; let* to lets
        ((let*? exp) (eval (let*->nested-lets exp) env))

        ((begin? exp) ;; tagged-list - begin
         (eval-sequence
          (begin-actions exp) env))

        ((cond? exp) ;; tagged-list - cond
         (eval
          (cond->if exp) env))

        ((do? exp)
         (eval
          (eval-do exp) env))

        ((application? exp) ;; function call
         (meta-apply
          (eval (operator exp) env)
          (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

;; REPL
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

; input driver loop
(define (driver-loop)
  (prompt-for-input input-prompt)
  ; read returns a list of the complete expression that the user types
  ; eg user types (+ 23 x) -> read returns a list with a SYMBOL, number and SYMBOL (+ 23 x)
  ; eg user types 'x -> read returns list with quote and symbol x (quote x)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; ; change syntax so that if true, evaluate to b in (if pred a b)
;; (define input-text
;;   '(if (= 1 1) 2 3)
;;   )

;; (define (one-shot)
;;   (let ((input input-text))
;;     (let ((output (eval input the-global-environment)))
;;       (announce-output output-prompt)
;;       (user-print output))
;;     )
;;   ;; (driver-loop)
;;   )

;; setup environment sets up the primitives like
;; car, cons, +, 'true
(define the-global-environment (setup-environment))
;; (one-shot)

(driver-loop)

;; run the driver loop
;; enter

;; (define x 0)
;; (do (set! x (+ x 1)) (< x 10))
;; x

;; you should see 10

