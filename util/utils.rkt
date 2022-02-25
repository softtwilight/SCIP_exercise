 #lang racket
(define (square x) (* x x))

(define (print x) (display x) (newline))

(define (avg x y) (/ (+ x y) 2))

(define (identity x) x)

(define % remainder)

(define (even? x) (= (% x 2) 0))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))

(define (!= a b) (not (= a b)))

(define (find-divisor next n a)
  (cond ((> (square a) n) n)
        ((= (% n a) 0) a)
        (else (find-divisor next n (next a)))))

(define (smallest-divisor next n)
  (find-divisor next n 2))

(define (prime? n)
  (define (next a)
    (if (= a 2)
        3
        (+ a 2)))
  (= n (smallest-divisor next n)))
                       
(provide (all-defined-out))