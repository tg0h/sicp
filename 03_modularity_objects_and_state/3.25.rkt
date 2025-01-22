#lang sicp

;; (define (lookup keys table)
;;   (let ((key (car keys))
;;         (is-last-key? (null? (cdr keys))))
;;     (let ((record (assoc key (cdr table))))
;;       (if record
;;           (if is-last-key?
;;               record
;;               (lookup (cdr keys) record))
;;           false
;;           ))))
;; (define (insert! keys value table)
;;   (define (create-table keys value)
;;     (cons (car keys)
;;           (if (null? (cdr keys))
;;               value
;;               (list (create-table (cdr keys) value)))))
;;   (if (null? keys) error "keys are empty")
;;   (let ((key (car keys))
;;         (is-last-key? (null? (cdr keys))))
;;     (let ((record (assoc key (cdr table))))
;;       (if record
;;           (if (is-last-key?)
;;               (set-cdr! record value)
;;               (insert! (cdr keys) value record))
;;           (set-cdr! table (cons
;;                            (create-table keys value)
;;                            value)))))
;;   'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys table)
      (display "lookup> keys:")
      (display keys)
      (display " table:")
      (display table)
      (newline)
      (let ((key (car keys))
            (is-last-key? (null? (cdr keys))))
        (let ((record (assoc key (cdr table))))
          (if record
              (if is-last-key?
                  (cdr record) ; return record value
                  (lookup (cdr keys) record))
              false
              ))))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (insert! keys value table)
      (display "insert! keys:")
      (display keys)
      (display " value: ")
      (display value)
      (display " table:")
      (display table)
      (newline)
      (define (create-table keys value)
        (cons (car keys)
              (if (null? (cdr keys))
                  value
                  (list (create-table (cdr keys) value)))))
      (if (null? keys) error "keys are empty")
      (let ((key (car keys))
            (is-last-key? (null? (cdr keys))))
        (let ((record (assoc key (cdr table))))
          (display "insert> key:")
          (display key)
          (display " record: ")
          (display record)
          (display " is-last-key?: ")
          (display is-last-key?)
          (newline)
          (if record
              (if (is-last-key?)
                  (set-cdr! record value)
                  (insert! (cdr keys) value record))
              (set-cdr! table (cons
                               (create-table keys value)
                               (cdr table))))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc)
         (lambda (keys) (lookup keys local-table)))
        ((eq? m 'insert-proc!)
         (lambda (keys value) (insert! keys value local-table)))
        (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; (put '(1) 2)
;; (get '(1 2))
;; (get '(1))

(put '(1 2) 3)
;; (get '(1))
(put '(1 3) 5)
;; (get '(1 2))
;; (put '(3 4) 5)
;; (get '(3 4))
;; (put '(4 5 6) 7)
;; (get '(4 5 6))

