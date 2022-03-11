 #lang sicp
(#%require "../../util/utils.rkt")

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))


;; a
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))
(define (mobile? x) (pair? x))

(define tree
  (make-mobile (make-branch 2 1)
               (make-branch 1
                            (make-mobile
                             (make-branch 2 2)
                             (make-branch 3 5)))))

;; b

(define (branch-weight b)
 (* (branch-length b)
    (total-weight (branch-structure b))))


  
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (mobile? mobile)) mobile)
        (else (+ (branch-weight (left-branch mobile))
                 (branch-weight (right-branch mobile))))))
                   

(total-weight tree)

;; c
(define (balance? mobile)
  (cond ((null? mobile) #t)
        ((not (mobile? mobile)) #t)
        (else (let ((left (left-branch mobile))
                    (right (right-branch mobile)))
                (and (balance? (branch-structure left))
                     (balance? (branch-structure right))
                      (= (branch-weight left)
                         (branch-weight right)))))))



(define balance-tree
  (make-mobile (make-branch 4 9)
               (make-branch 3
                            (make-mobile
                             (make-branch 2 3)
                             (make-branch 1 6)))))

(balance? tree)
(balance? balance-tree)
    
        