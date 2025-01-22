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
      (let ((key (car keys))
            (is-last-key? (null? (cdr keys))))
        (let ((record (assoc key (cdr table))))
          (if record
              (if is-last-key?
                  record
                  (lookup (cdr keys) record))
              false
              ))))
    (define (insert! keys value table)
      (define (create-table keys value)
        (cons (car keys)
              (if (null? (cdr keys))
                  value
                  (list (create-table (cdr keys) value)))))
      (if (null? keys) error "keys are empty")
      (let ((key (car keys))
            (is-last-key? (null? (cdr keys))))
        (let ((record (assoc key (cdr table))))
          (if record
              (if (is-last-key?)
                  (set-cdr! record value)
                  (insert! (cdr keys) value record))
              (set-cdr! table (cons
                               (create-table keys value)
                               value)))))
      'ok)
    (define (dispatch m)
      (cond (
             ((eq? m 'lookup-proc)
              (lambda (keys) (lookup keys local-table)))
             ((eq? m 'insert-proc!)
              (lambda (keys value) (insert! keys value local-table)))
             )
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put '(1 2) 3)
