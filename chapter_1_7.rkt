#lang sicp

;;exercise 1.43
(define (repeated f n)
  (if (= n 1)
      f
     (repeated (lambda (x) (f (f x))) (- n 1))))
(define (square x) (* x x))
((repeated square 2) 3)


;;exercise 1.44
(define dx 0.00001)
(define (smoothing f)
  (lambda (x) (/ (+
                  (f x)
                  (f (+ x dx))
                  (f (- x dx)))
                 3)))
(((repeated smoothing 2) square) 2.6)

;; exercise 1.45
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

;; compute a * a  b times
(define (square-n a b)
  (define (iter x n res)
    (if (= n 0)
        res
        (iter x (- n 1) (* res x))))
  (iter a b 1))

(define (repeated f n)
  (if (= n 1)
      f
     (repeated (lambda (x) (f (f x))) (- n 1))))

(define (nth-sqrt x n)
  ;; repeat the average-damp (floor log(2 n)) times
  (fixed-point ((repeated average-damp (floor (log n 2)))
                 (lambda (y)
                   (/ x (square-n y (- n 1)))
                ))
               1.0))

(nth-sqrt 512 9) ;; -> 2.000039026405641