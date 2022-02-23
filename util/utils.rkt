 #lang racket
(define (square x) (* x x))

(define (print x) (display x) (newline))

(define (avg x y) (/ (+ x y) 2))
                       
(provide (all-defined-out))