#lang racket
(#%require "../../util/utils.rkt")

;; set is ordered
;; can't use this method for accumulate
;; because we need to update set in every operation.
(define (sub-set-le x set)
  (define (iter x result remain)
    (cond ((null? remain) (append result (list x)))
          ((< x (car remain)) (append result (list x)))
          ((= x (car remain)) (append result (list x)))
          ((> x (car remain)) (iter x
                                    (append result (list (car remain)))
                                    (cdr remain)))))
  (iter x '() set))

(null? (list 1 2))

(sub-set-le 3 (list 1 2 4 5))    

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))))))))

(union-set (list 1 3 5) (list 2 4 6))

(union-set (list 1 3 5) (list))

(union-set (list 1 3 5) (list 6 7))