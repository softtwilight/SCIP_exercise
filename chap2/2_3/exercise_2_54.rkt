#lang racket
(define (equal? list-a list-b)
  (cond ((and (not (pair? list-a)) (not (pair? list-b)))
         (eq? list-a list-b))
        ((and (pair? list-a) (pair? list-b))
         (and (equal? (car list-a) (car list-b))
              (equal? (cdr list-a) (cdr list-b))))
        (else 'f)))

(equal? '(a 2) '(a 2))
(equal? '(a (list 1 2 'c) 2) '(a (list 1 2 'c) 2))