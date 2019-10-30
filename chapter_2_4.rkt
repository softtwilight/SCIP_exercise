#lang sicp
;; exercise_2.11
;; the difficult here is has 9 conditions, how to
;; write simple and clearly enough to cover these condition

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (car x)))

(define (0ld-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
  (let ((x-neg (< x2 0))
        (x-pos (> x1 0))
        (y-neg (< y2 0))
        (y-pos (> y1 0)))
  (cond (x-neg
         (cond (y-neg (make-interval (* x2 y2) (* x1 y1)))
               (y-pos (make-interval (* x1 y2) (* x2 y1)))
               (else (make-interval (* x1 y2) (* x1 y1)))))
        (x-pos
         (cond (y-neg (make-interval (* x2 y1) (* x1 y2)))
               (y-pos (make-interval (* x1 y1) (* x2 y2)))
               (else (make-interval (* x2 y1) (* x2 y2)))))
        (else
         (cond (y-neg (make-interval (* x2 y1) (* x1 y1)))
               (y-pos (make-interval (* x1 y2) (* x2 y2)))
               (else (make-interval (min (* x1 y2) (* x2 y1)) 
                                              (max (* x1 y1) (* x2 y2))))))))))

;; test generate
(define (generate-intervals) 
  (define test-list '()) 
  (define test-data 
    (cons (list 0 1 2 3 4 5 -6 -7 -8 -9 -10) 
          (list 5 4 3 2 1 0 -1 -2 -3 -4 -5))) 
  (for-each 
   (lambda (x) (set! test-list (append test-list x))) 
   (map    (lambda (x)     (map    (lambda (y) (make-interval x y)) 
                                   (cdr test-data))) 
           (car test-data))) 
  (cons test-list test-list)) 
  
(define test-intervals 
  (generate-intervals)) 
  
(define (test f g) 
  (define (interval-equals a b) 
    (and (= (lower-bound a) (lower-bound b)) (= (upper-bound a) (upper-bound b)))) 
  (for-each (lambda (x) 
              (for-each (lambda (y) 
                          (cond   ((interval-equals (f x y) (g x y)) #T) 
                                  (else 
                                   (newline) 
                                   (display x) (display y) 
                                   (newline) 
                                   (display (f x y)) (display (g x y)) 
                                   (newline)))) 
                        (cdr test-intervals))) 
            (car test-intervals))) 
  
 (define (old-mul-interval x y) 
         (let            ((p1 (* (lower-bound x) (lower-bound y))) 
                          (p2 (* (lower-bound x) (upper-bound y))) 
                          (p3 (* (upper-bound x) (lower-bound y))) 
                          (p4 (* (upper-bound x) (upper-bound y)))) 
                 (make-interval 
                         (min p1 p2 p3 p4) 
                         (max p1 p2 p3 p4)))) 


 (define (display-interval i) 
   (newline) 
   (display "[") 
   (display (lower-bound i)) 
   (display ",") 
   (display (upper-bound i)) 
   (display "]")) 
(test mul-interval 0ld-mul-interval)
