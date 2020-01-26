#lang sicp

;; exercise_2.21
(define nil '())
(define (square x) (* x x))
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))
(define (square-list items)
  (map square items))
(square-list1 (list 1 2 3 4))
(square-list (list 1 2 3 4))

;; exercise_2.22
;; (cons list num) will not add the second argument to list
;; can use append except (append list (list num))

;; exercise_2.23
;; begin can sequentially execute multiple code
(define (cus-for-each func list)
  (if (not (null? list))
      (begin
       (func (car list))
      (cus-for-each func (cdr list)))))

(cus-for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))


(list 1 (list 2 (list 3 4)))

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

;; exercise_2.27

(define (reverse li)
  (define (inner-rev nl ol)
    (if (null? ol)
        nl
        (inner-rev (cons (car ol) nl) (cdr ol))))
  (inner-rev nil li))
(define items (list (list 1 2) (list 3 4)))
(reverse items)


(define (deep-reverse x)
  (define (inner-cur nl ol)
    (if (null? ol) nl
        (inner-cur (cons
                    (if (pair? (car ol))
                        (deep-reverse (car ol))
                        (car ol))
                    nl) (cdr ol)
                  )))
  (inner-cur nil x))
(define items2 (list (list 1 2) (list 3 4) (list (list 5 6) 7)))
(define items3 (list items items2))
(deep-reverse items)
(deep-reverse items2)
(deep-reverse items3)
(deep-reverse (list 4 5 7))