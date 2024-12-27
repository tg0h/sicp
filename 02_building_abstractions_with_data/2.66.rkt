#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (make-record key value)
  (list key value))

(define (key record)
  (car record)
  )

(define tim-record (make-record 1 'tim))
(define joop-record (make-record 2 'joop))
(define max-record (make-record 3 'max))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n) (if (= n 0)
                                  (cons '() elts)
                                  (let ((left-size (quotient (- n 1) 2)))
                                    (let ((left-result
                                           (partial-tree elts left-size)))
                                      (let ((left-tree (car left-result))
                                            (non-left-elts (cdr left-result))
                                            (right-size (- n (+ left-size 1))))
                                        (let ((this-entry (car non-left-elts)) (right-result
                                                                                (partial-tree
                                                                                 (cdr non-left-elts)
                                                                                 right-size)))
                                          (let ((right-tree (car right-result)) (remaining-elts
                                                                                 (cdr right-result)))
                                            (cons (make-tree this-entry
                                                             left-tree
                                                             right-tree)
                                                  remaining-elts))))))))

(define db (list->tree (list tim-record joop-record max-record)))
db


(define (_lookup given-key set-of-records)
  (cond
    ((null? set-of-records) false)
    ((= (key (entry set-of-records)) given-key) (entry set-of-records))
    ((< given-key (key (entry set-of-records))) (_lookup given-key (left-branch set-of-records)))
    (else (_lookup given-key (right-branch set-of-records))
          ))
  )

(_lookup 1 db)
(_lookup 2 db)
(_lookup 3 db)
