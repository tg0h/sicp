#lang sicp

(define (make-queue)
  (define q (cons '() '()))
  (let ((front-ptr
         (lambda ()
           (car q))
         )
        (rear-ptr (cdr q)))
    (define (front-queue)
      (display "front-q")
      (if (empty-queue?)
          (error "FRONT called with an empty queue" q)
          ;; ("ha")
          (car (front-ptr))
          ))
    (define (set-front-ptr! item) (set-car! q item))
    (define (set-rear-ptr! item) (set-cdr! q item))
    (define (empty-queue?)
      ;; (display (null? (front-ptr)))
      (display "EMPTY_Q")
      ;; 1
      (front-ptr)
      ;; (null? (front-ptr))
      )
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr!  new-pair)
               (set-rear-ptr!  new-pair)
               q)
              (else
               (set-cdr! (rear-ptr ) new-pair)
               (set-rear-ptr!  new-pair)
               q))))
    (define (delete-queue! )
      (cond ((empty-queue? )
             (error "DELETE! called with an empty queue" q))
            (else (set-front-ptr! q (cdr (front-ptr)))
                  q)))
    (define (dispatch m)
      (display "CALLING DISPATCH")
      ((cond
         ((eq? m 'front-queue) front-queue)
         ((eq? m 'insert-queue!) insert-queue!)
         ((eq? m 'delete-queue!) delete-queue!)
         )
       )
      )
    (display "tim")
    dispatch
    )
  )

(define z (make-queue))
z

(z 'front-queue)
;; ((z 'insert-queue!) 'a)
;; (define q1 (make-queue))
;;
;; (insert-queue! q1 'a)
;; (print-queue q1)
;;
;; (insert-queue! q1 'b)
;; (print-queue q1)
;;
;; (delete-queue! q1 )
;; (print-queue q1)
;; (delete-queue! q1 )
;; (print-queue q1)
;;
