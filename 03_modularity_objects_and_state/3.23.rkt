#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-dequeue queue)
  (if (empty-queue? queue)
      (error "FRONT-dequeue called with an empty queue" queue)
      (car (front-ptr queue))))

(define (rear-dequeue queue)
  (if (empty-queue? queue)
      (error "REAR-dequeue called with an empty queue" queue)
      (car (rear-ptr queue))))

(define (make-item item)
  (list item '() '())
  )
(define (set-next-item! a b) (set-car! (cdr a) b))
(define (set-previous-item! a b) (set-car! (cddr a) b))
(define (join-item a b)
  (set-next-item! a b)
  (set-previous-item! b a)
  )
(define (previous-item a) (caaddr a))
(define (next-item a) (caadr a))
(define (next-item-pointer item) (cadr item))
(define (previous-item-pointer item) (caddr item))
(define (is-next-item-nil? item) (null? (next-item-pointer item)))
(define (is-previous-item-nil? item) (null? (previous-item-pointer item)))

(define (insert-queue! queue item)
  (let ((new-item (make-item item)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           ;; (set-cdr! (rear-ptr queue) new-item)
           (join-item (rear-ptr queue) new-item)
           (set-rear-ptr! queue new-item)
           queue))))

(define (front-delete-dequeue! queue)
  (cond ((empty-queue? queue)
         (error "front-DELETE! called with an empty queue" queue))
        ((eq? (front-ptr queue) (rear-ptr queue))
         (set-front-ptr! queue
                         ;; (next-item-pointer (front-ptr queue))
                         nil
                         )
         (set-rear-ptr! queue
                        ;; (next-item-pointer (front-ptr queue))
                        nil
                        )
         queue
         )
        (else
         (set-previous-item! (next-item-pointer (front-ptr queue)) nil) ; delink 2nd item from 1st item
         (set-front-ptr! queue (next-item-pointer (front-ptr queue))) ; point front-ptr to 2nd item
         queue)))

(define (rear-delete-dequeue! queue)
  (cond ((empty-queue? queue)
         (error "rear-DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define (print-queue q)
  (display "PRINT: ")
  (display (front-ptr q))
  (newline)
  )

(define q1 (make-queue))

(insert-queue! q1 'a)
(front-dequeue q1)
;; (rear-dequeue q1)
;; (print-queue q1)

(insert-queue! q1 'b)
(front-dequeue q1)
(rear-dequeue q1)

;; (front-delete-dequeue! q1 )
;; (print-queue q1)
;; (front-delete-dequeue! q1 )
;; (print-queue q1)

