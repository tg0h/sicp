#lang sicp

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq arrow-test)
  (cond ((null? seq) seq) ; if there is no expression in the cond clause, then return nil
        ((last-exp? seq) (first-exp seq)) ; if there is only 1 expression, then return that expression
        ; very bad style, this becomes an application
        ((eq? (car seq) '=>) (eval (cons arrow-test (cond-arrow-recipient seq))))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; cond (convert cond to nested ifs)
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond-arrow-test clause) (car clause))
(define (cond-arrow-operator clause) (cadr clause))
(define (cond-arrow-recipient clause) (caddr clause))

(define (cond-arrow-clause? clause)
  ;; (
  ;;   (assoc 'b '((a 1) (b 2)))
  ;;   =>
  ;;   cadr
  ;; )
  (and (= (length clause) 3)
       (eq? (cond-arrow-operator clause) '=>)
       )
  )


(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        ;; (if
        ;;  (and
        ;;   (cond-arrow-clause? first) ; if cond arrow clause
        ;;   (cond-arrow-test) ; and test evaluates to true
        ;;   )
        ;;  (cond-arrow-recipient (cond-arrow-test))
        (if (cond-else-clause? first)
            (if (null? rest) ; the else condition does not have any other predicates below
                (sequence->exp (cond-actions first) nil)
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first) ; if predicate satisfied, evaluate the cond actions
                     (sequence->exp (cond-actions first) (cond-predicate first) )
                     (expand-clauses rest))))))

; test predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))
