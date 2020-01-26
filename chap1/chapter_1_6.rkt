#lang sicp

;; fixed-point find
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
(lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (* y y) x)) 1.0))

;; exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) ( * a x x) (* b x) c)))
(define a 1)
(define b 1)
(define c 1)
(display (newtons-method (cubic a b c) 1))

;; exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
((double inc) 2) ;; -> 4
(((double (double double)) inc) 5) ;; -> 21

;;1.42
(define (square x) (* x x))
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6) ;; -> 49
