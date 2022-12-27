#lang racket

(define (element- of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (con x set))
(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (accumulate (lambda (x result)
                (if (element-of-set? x set2)
                    (cons x result)
                    result))
              '()
              set1))

;; although the set has 3 elements, but it may take much longer operation time