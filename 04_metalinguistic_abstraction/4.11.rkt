#lang sicp

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values) (map (lambda (x y) (cons x y)) variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val env)
  (set-car! env (cons (cons var val) (car env))
            ))

(define (first-binding bindings) (car bindings))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings) ; bindings is a list of var val pairs
      (if (null? bindings)
          (env-loop (enclosing-environment env))
          (let ((binding (first-binding bindings)))
            (cond
              ((eq? var (car binding)) (cdr binding))
              (else (scan (cdr bindings)))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable lolz" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings) ; bindings is a list of var val pairs
      (if (null? bindings)
          (env-loop (enclosing-environment env))
          (let ((binding (first-binding bindings)))
            (cond
              ((eq? var (car binding)) (set-cdr! binding val))
              (else (scan (cdr bindings)))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan f)
      (if (null? f)
          (add-binding-to-frame! var val env)
          (let ((binding (car f)))
            (display "var: ")
            (display var)
            (display " check")
            (display (car binding))
            (newline)
            (cond ((eq? var (car binding)) (set-cdr! binding val))
                  (else (scan (cdr f)))))))
    (scan frame)))

(define frame
  (list
   (cons 'v8 'l8)
   (cons 'v9 'l9)
   )
  )

(define frame2 (list (cons 'v3 'l3) (cons 'v4 'l4)))
;; (define null-frame (list))

(define env (list  frame ))
;; (define env (list  frame ))

;; (define vs (list 'v1 'v2))
;; (define ls (list 'l1 'l2))

;; env
;; (extend-environment vs ls env)

;; (make-frame vs ls)
;; (lookup-variable-value 'v3 env)


env
(define-variable! 'v1 'l1 env)
env
;; (define z (list frame2))

;; (define z (list 1 2))
;; (define y z)
;; (cons 0 z)
;; (set-car! y (cons 0 z))
;; y
;; z
;; z
;; y

