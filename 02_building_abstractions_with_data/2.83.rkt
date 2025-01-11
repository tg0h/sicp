#lang sicp




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
(define coerce-table (make-table))
(define get-coercion (coerce-table 'lookup-proc))
(define put-coercion (coerce-table 'insert-proc!))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum) (if (pair? datum)
                             (car datum)
                             (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum) (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define tower '(integer rational real complex))

(define (next-type type)
  (define (loop type type-list)
    (cond
      ((eq? type 'complex) (error "unable to raise complex"))
      ((eq? type (car type-list)) (cadr type-list))
      (else (loop type (cdr type-list)))
      )
    )
  (loop type tower)
  )

(next-type 'complex)
(define (raise number)
  (let ((type (type-tag number)))
    (let ((raise-type (next-type type)))
      (let ((type->raise-type (get-coercion type raise-type)))
        (type->raise-type number)
        )
      )
    )
  )

;; (get-coercion type raise-type)
