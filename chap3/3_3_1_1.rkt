#lang sicp
;; 3.3 Modeling with Mutable Data

;; 3.3.1 Mutable List Structure
;; set-car! first arg is the pair, second arg is the new pointer to value.

;; exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x) ;; -> b
(define w (append! x y))
w
(cdr x) ;; -> (b c d)

;; exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z2 (make-cycle (list 'a 'b 'c)))
;; compute (last-pair z) will run forever. the (cdr z) always return a pointer.

;; exercise 3.14
;; in-place reverse.
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))
(define li (list 'a 'b 'c 'd))
(define w2 (mystery li))
li
w2