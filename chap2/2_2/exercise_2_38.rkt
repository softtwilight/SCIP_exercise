#lang racket
(#%require "../../util/utils.rkt")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; if A op B = B op A, accumulate result equals fold-left
(accumulate * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
    