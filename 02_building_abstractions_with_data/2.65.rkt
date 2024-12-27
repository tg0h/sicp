#lang sicp

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set) (cond ((null? set) false)
                                      ((= x (entry set)) true)
                                      ((< x (entry set))
                                       (element-of-set? x (left-branch set)))
                                      ((> x (entry set))
                                       (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))


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

;; (partial-tree (list 1 3 5 7 9 11) 6)

(define set1 (list->tree (list 1 2 3)))

(define set2 (list->tree (list 3 4 5)))

(tree->list-2 set1)


(define (_union-set set1 set2)
  (cond
    ((null? set2) set1)
    ((null? set1) set2)
    ((= (car set1) (car set2)) (cons (car set1) (_union-set (cdr set1) (cdr set2))))
    ((< (car set1) (car set2)) (cons (car set1) (_union-set (cdr set1) set2)))
    (else
     (cons (car set2) (_union-set set1 (cdr set2)))
     )
    )
  )

(define (union-set s1 s2)
  (let (
        (l1 (tree->list-2 s1))
        (l2 (tree->list-2 s1))
        )
    (let ((ul (_union-set l1 l2)))
      (list->tree ul)
      )
    )
  )

(union-set set1 set2)
