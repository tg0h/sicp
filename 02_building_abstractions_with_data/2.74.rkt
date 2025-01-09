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
  (define (get-employee-value employee-record key)
    (define (loop key employee-values)
      (cond
        ((null? employee-values) nil)
        (else
         (let ((kv (car employee-values)))
           (if (eq? key (car kv))
               (cdr kv) ;return val if match
               (loop key (cdr employee-values))))))) ;else continue searching
    (if (null? employee-record ) nil
        (loop key (cdr employee-record))) ; start looping
    )
  (define (get-salary name)
    (get-employee-value (get-record name) 'salary)
    )
  (define (get-record name)
    (define (iter name file)
      (if (null? file) nil
          (let
              ((record (car file)))
            (cond
              ((null? file) nil)
              ((eq? name (car record)) record)
              (else (iter name (cdr file)))
              )
            )
          )
      )
    (iter name division-a-file)
    )
  (put 'get-record 'division-a get-record)
  (put 'get-salary 'division-a get-salary)
  ; install seed data
  (add-employee-record 'tim 1 'sg)
  (add-employee-record 'jo 2 'uk)
  )
(install-division-a)


(define (make-division-file type ) (list type))
(define (get-division-type file) (car file))

(define division-a-file (make-division-file 'division-a))


(define (get-record file employee-name)
  ((get 'get-record (get-division-type file)) employee-name)
  )

(define (get-salary file employee-name)
  ((get 'get-salary (get-division-type file)) employee-name)
  )

(get-record division-a-file 'tim)
(get-salary division-a-file 'tim)

(define (find-employee-record files name)
  (define (loop name files)
    (let ((f (car files)))
      (let ((emp (get-record f name)))
        (if (null? emp)
            (loop name (cdr files))
            emp)
        )))
  (if (null? files)
      nil
      (loop name files))
  )


;; (get-record (list 'division-a) 'tim)
(find-employee-record (list (list 'division-a)) 'tim)


