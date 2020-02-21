#lang sicp
;; (eq? x y) to test whether two symbols or pointer are equal.
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)
;; exercise 3.15
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(set-to-wow! z2)

;; exercise 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define p1 (cons 'c '()))
(define p2 (cons 'b p1))
(define p3 (cons 'a p2))
(define p22 (cons p1 p1))
(define p4 (cons 'a p22))
(define p7 (cons p22 p22))

 (count-pairs p3) ;; -> 3
(count-pairs p4) ;; -> 4
(count-pairs p7) ;; -> 7
(define never-return
  (begin (set-cdr! p2 p3)
         p2))
;; (count-pairs never-return) ;; -> never return.

;; exercise 3.17
;; memq function of element is in list
(define (count-real-pairs x)
  (define visited '())
  (define (travel x)
    (if (or (not (pair? x)) (memq x visited))
        0
        (begin
          (set! visited (cons x visited))
          (+ (travel (car x))
             (travel (cdr x))
             1))))
  (travel x))


    
(count-real-pairs p3) ;; -> 2
(count-real-pairs p4) ;; -> 3
(count-real-pairs p7) ;; -> 3
(count-real-pairs never-return) ;; -> 2


;; exercise 3.8
(define (contains-cycle x)
  (define visited '())
  (define (travel x)
    (if (not (pair? x))
        #f
        (if (memq x visited)
            #t
            (begin (set! visited (cons x visited))
                   (travel (cdr x))))))
  (travel x))
(contains-cycle p7) ;; -> #f
(contains-cycle never-return)  ;; -> #t

;; exercise 3.9
;; takes only a constant amount of space.
(define (double-cdr x)
  (if (null? (cdr x))
      '()
      (cddr x)))
(define (contains-c x)
  (define (travel x y)
    (if (null?  x)
        #f
        (if (eq? (car x) (car y))
            #t
            (travel (double-cdr x) (cdr y)))))
  (travel (cdr x) x))
(contains-c p7)
(contains-c never-return)

     