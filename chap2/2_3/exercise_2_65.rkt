#lang racket
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


(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  ;; ...
  elts)


(define (union-set s1 s2)
  (list->tree
   ;; O(n), because only need transverse two set once
   (adjoin-order-list-all
    (tree->list-2 s1)
    (tree->list-2 s2))))
(define (intersection-set s1 s2)
  (list->tree
   ;; O(n), because only need transverse two set once
   (adjoin-order-list-same
    (tree->list-2 s1)
    (tree->list-2 s2))))

  