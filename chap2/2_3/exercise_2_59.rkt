#lang racket
(#%require "../../util/utils.rkt")

;; less effective than book version. 2 times complexity
(define (element-of-set? x set)
  (accumulate (lambda (a b) (or a b))
              #f
              (map (lambda (s) (equal? s x))
                   set)))

(element-of-set? 1 (list 1 2 3))
(element-of-set? 1 (list))
(element-of-set? 1 (list '1))
(element-of-set? 1 (list 2 3))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 1 (list 1 2 3))
(adjoin-set 0 (list 1 2 3))


;; use accumulate to define intersection.
(define (intersection-set set1 set2)
  (accumulate (lambda (x result)
                (if (element-of-set? x set2)
                    (cons x result)
                    result))
              '()
              set1))


;; 2.59
(define (union-set set1 set2)
  (accumulate adjoin-set
              set2
              set1))

(union-set (list 1 2 3) (list 3 4 5))
(union-set '() (list 3 4 5))
(union-set '(1 2 3) '())