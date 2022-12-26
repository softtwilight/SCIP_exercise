#lang racket
((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))

((exponentiation? exp)
 (make-product
  (make-product (exponent exp)
                (make-exponentiation (base exp) (- (exponent exp) 1)))
  (deriv (base exp) var)))



(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base p) (cadr p))
(define (exponent p) (caddr p))
(define (make-exponentiation e1 e2)
  (cond ((number? e1 1) 1)
        ((number? e2 0) 1)
        ((number? e2 1) e1)
        (else (list '** e1 e2))))