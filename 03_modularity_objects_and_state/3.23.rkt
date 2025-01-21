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

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (front-delete-dequeue! queue)
  (cond ((empty-queue? queue)
         (error "front-DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
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
(rear-dequeue q1)
;; (print-queue q1)

(insert-queue! q1 'b)
;; (print-queue q1)

(front-delete-dequeue! q1 )
;; (print-queue q1)
(front-delete-dequeue! q1 )
;; (print-queue q1)

