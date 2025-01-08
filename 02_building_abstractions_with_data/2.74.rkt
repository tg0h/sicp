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

(define (get-record division employee-name)
  ((get 'get-record division) employee-name)
  )


;; (get-record 'division-a 'tim)

(define (install-division-a)
  (define division-a-file '())
  (define (make-employee-record name salary country)
    (list name (cons 'salary salary) (cons 'country country))
    )
  (define (add-employee-record name salary country)
    (let ((record (make-employee-record name salary country)))
      (set! division-a-file (cons record division-a-file))
      )
    )
  (define (find-employee-record name)
    (define (iter name file)
      (if (null? file) nil
          (let
              ((record (car file)))
            (cond
              ((null? file) nil)
              ((eq? name (car record)) record)
              (else iter name (cdr file))
              )
            )
          )
      )
    (iter name division-a-file)
    )
  (put 'get-record 'division-a get-record)
  ; install seed data
  (add-employee-record 'tim 1 'sg)
  (add-employee-record 'jo 2 'uk)
  )

(install-division-a)
(get-record 'division-a 'tim)
