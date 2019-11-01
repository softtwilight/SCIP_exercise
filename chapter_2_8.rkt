#lang sicp

;; exercise_2.28
;; similar as count-leaf
(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

;; exercise_2.29
;; the key of point here is use two function to generate a recursive
;; structure a -> b, b -> a, two function call each other recursively.

;; part.a
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch x)
  (car x))
(define (right-branch x)
  (car (cdr x)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))

(define mobile0 (make-mobile (make-branch 10 25)
                                  (make-branch 5 20)))
(left-branch mobile0)
(right-branch mobile0)
(branch-length (right-branch mobile0))
(branch-structure (right-branch mobile0))

;; part.b
(define (branch-weight b)
  (if (pair? (branch-structure b))
      (total-weight (branch-structure b))
      (branch-structure b)))

(define (total-weight m)
  (if (null? m)
      0
      (+ (branch-weight (left-branch m))
         (branch-weight (right-branch m)))))

(total-weight mobile0) ;; -> 45
(define another-mobile (make-mobile (make-branch 10 mobile0)
                                    (make-branch 10 20)))
(total-weight another-mobile) ;; -> 65

;; part.c
(define (branch-product b)
  (* (branch-length b) (branch-weight b)))

(define (test-branch b)
  (if (pair? (branch-structure b))
      (test-mobile (branch-structure b))
      #T))
  
(define (test-mobile m)
  (let ((lb (left-branch m))
        (rb (right-branch m)))
  (and (= (branch-product lb)
              (branch-product rb))  
       (test-branch lb)
       (test-branch rb))))

(define mobile1 (make-mobile (make-branch 6 8)
                                  (make-branch 3 16)))
(define mobile2 (make-mobile (make-branch 2 24)
                                  (make-branch 12 4)))
(define mobile3 (make-mobile
                 (make-branch 1 mobile1)
                 (make-branch 1 24)))
                                  
(test-mobile mobile0) ;; -> false
(test-mobile mobile1) ;; -> true
(test-mobile mobile2) ;; -> true
(test-mobile mobile3) ;; -> true
  
;; part.d
;; the benefit of layer abstraction, we only need two change to selector function
(define (make-mobile left right)
   (cons left right))
(define (make-branch length structure)
   (cons length structure))