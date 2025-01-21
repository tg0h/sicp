#lang sicp


;; (define (make-table)
;; (list '*table*))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (insert! key value )
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))
    )
  )

(define (within-5? k1 k2) (<= (abs (- k1 k2)) 5))

(define operation-table (make-table within-5?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; ((operation-table 'insert-proc!) 10 'tim)

(put 10 'tim)
(get 10)
(get 9)
(get 5)
(get 4)
