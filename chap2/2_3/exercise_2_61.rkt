#lang racket


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 2 (list 1 3 4 2))


;; Θ(n) growth, only when x in set, will smaller than n
;; othertime is n.
;; we need to maintain the order
(define (adjoin-set x set)
  (define (iter x result remain)
    (cond ((null? remain) (append result (list x)))
          ((= x (car remain)) set)
          ((< x (car remain)) (append result (list x) remain))
          (else (iter x
                      (append result (list (car remain)))
                      (cdr remain)))))
  (iter x '() set))
(adjoin-set 3 (list 1 5 9))
(adjoin-set -1 (list 1 5 9))
(adjoin-set 9 (list 1))
(adjoin-set 9 (list))