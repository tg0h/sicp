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
  (define division-a-file (list (list 'tim 1)))
  (define (get-record employee-name)
    ;; ...
    (display employee-name)
    (newline)
    (display "ok")
    )
  (put 'get-record 'division-a get-record)
  )

(install-division-a)
(get-record 'division-a 'tim)
