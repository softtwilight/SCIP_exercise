#lang sicp

;; exercise_2.1

(define (make-rat n d) 
   (define (sign x) (if (< x 0) - +)) 
   (let ((g (gcd n d))) 
     (cons ((sign d) (/ n g)) 
               (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(print-rat (make-rat -1 2)) ;; -> -1/2
(print-rat (make-rat 1 -2)) ;; -> -1/2
(print-rat (make-rat -1 -2)) ;; -> 1/2

;; exercise_2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (average a b)
  (/ (+ a b) 2))
(define (average-point a b)
  (make-point (average (x-point a) (x-point b))
              (average (y-point a) (y-point b))))

(define (midpoint-segment s)
  (average-point (start-segment s)
                 (end-segment s)))

(print-point (midpoint-segment
              (make-segment (make-point 2 0) (make-point 0 4))));; -> (1,2)

 (newline)
 (newline)
;; exercise_2.3

(define (square x) (* x x))
(define (line-len p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

(define (seg-len s)
  (line-len (start-segment s) (end-segment s)))
(define (make-rect s1 s2) (cons s1 s2))
(define (length-rect r) (seg-len (car r)))
(define (width-rect r) (seg-len (cdr r)))

(define (area-rect r)
  (* (length-rect r) (width-rect r)))
(define (perimeter-rect r)
  (* (+ (length-rect r) (width-rect r)) 2))

(define point1 (make-point 0 0))
(define point2 (make-point 2 0))
(define point3 (make-point 0 4))
(define point4 (make-point 2 4))

(define cusLength (make-segment point1 point2))
(define cusWidth (make-segment point1 point3))

(define rect-cus (make-rect cusLength cusWidth))
(area-rect rect-cus);; -> 8
(perimeter-rect rect-cus);; -> 12