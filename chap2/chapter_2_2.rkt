#lang sicp

;;exercise_2.4
(define (cons2 x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
(define double-p2 (cons2 1 5))
(car double-p2) ;; -> 1
(cdr double-p2) ;; -> 5

;; exercise_2.5

(define (exp base n)
  (define (iter x result)
    (if (= x 0)
        result
        (iter (- x 1) (* base result))))
  (iter n 1))
(define (divides? a b)
  (= (remainder a b) 0))
(define (largest-power-divisor a s)
  (define (iter n result)
    (if (divides? result s)
        (iter (+ n 1) (/ result s))
        n))
   (iter 0 a)) 
(define (cons3 x y)
  (* (exp 2 x)
     (exp 3 y)))
(define (car3 z)
  (largest-power-divisor z 2))
(define (cdr3 z)
  (largest-power-divisor z 3))

(car3 (cons3 12 34)) ;; -> 12
(cdr3 (cons3 12 34)) ;; -> 34

;;exercise 2.6
;; the n means give a function and an argument,
;; apply the function to argument n times
;; so add-1 n means apply n + 1 times
(define zero
  (lambda (f)
    (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))
(define one
  (lambda (f) 
    (lambda (x) (f x))))
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)))))
(define three (add one two))
(define four (add one three))
(define seven (add three four))

((seven (lambda (x)
          (display x)
          (newline)
          x))
 "hello") ;; -> print hello times

((two inc) 2)

  

