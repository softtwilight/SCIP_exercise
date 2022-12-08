#lang racket
(define (square x) (* x x))

(define (print x) (display x) (newline))

(define (avg x y) (/ (+ x y) 2))
(define (inc i) (+ 1 i))

(define (identity x) x)

(define % remainder)

(define (cube x) (* x x x))

(define (even? x) (= (% x 2) 0))
(define (odd? x) (not (even? x)))

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


(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (avg y (/ x y))) 1.0))

(define (compose g f)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (cond ((= n 1) f)
        ((even? n) (repeat (compose f f) (/ n 2)))
        (else (compose f (repeat f (- n 1))))))

(define (assertEqual expect actual)
  (if (= expect actual)
      (print "success")
      (begin
        (display "expect: ") (display expect) (display ";actual: ") (display actual)(newline))))

(define nil '())

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))


(provide (all-defined-out))