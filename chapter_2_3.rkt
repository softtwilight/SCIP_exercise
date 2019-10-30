#lang sicp

;; exercise_2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (car x)))

 (define (display-interval i) 
   (newline) 
   (display "[") 
   (display (lower-bound i)) 
   (display ",") 
   (display (upper-bound i)) 
   (display "]")) 

;; exercise_2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define i1 (make-interval 1 3))
(define i2 (make-interval -5 4))
(display-interval (sub-interval i1 i2 )) ;; -> [-3,8]

;; exercise_2.9
(define (width-inter x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))
(newline)
(+ (width-inter i2) (width-inter i1))
(width-inter (sub-interval i1 i2 )) ;; -> 5.5

;; exercise_2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (check-span-zero y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))
(define (check-span-zero y)
  (if (and (>= (upper-bound y) 0)
          (<= (lower-bound y) 0))
      (error "error! can't span zero")))


(define i3 (make-interval -5 -1))
(display-interval (div-interval i1 i3 ));; -> [-3.0,-0.2]
