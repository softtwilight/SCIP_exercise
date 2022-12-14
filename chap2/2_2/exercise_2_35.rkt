#lang racket
(#%require "../../util/utils.rkt")

(define (count-leaves-normal t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves-normal (car t))
                 (count-leaves-normal (cdr t))))))

(assertEqual (count-leaves-normal '()) 0)
(assertEqual (count-leaves-normal (list 1 2 3)) 3)
(assertEqual (count-leaves-normal (list (list 1 2) (list 1 2 3) 1)) 6)


;; The key is wishful thinking.
;; map op will map a tree to a list of number, each number means how many leaves each sub tree have.
;; which is count-leaves exactly doing.
;; for example.
;; (define tree (list 1 2 (list 3 4) (list 5 (list 6 7))))
;; map op to the tree => (list 1 1 2 3)
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                 (cond
                   ((null? x) 0)
                   ((not (pair? x)) 1)
                   (else  (count-leaves x))))
                     t)))


(assertEqual (count-leaves '()) 0)
(assertEqual (count-leaves (list 1)) 1)
(assertEqual (count-leaves (list 1 2 3)) 3)
(assertEqual (count-leaves (list (list 1 2) (list 1 2 3) 1)) 6)


;; this version is easier to understand
;; it's define the operation of accumulate
;; instead of define list of datastructure above.
 (define (count-leaves-without-map t) 
   (accumulate (lambda (node count-thus-far) 
                 (+ count-thus-far 
                    (cond ((null? node) 0) 
                          ((not (pair? node)) 1) 
                          (else 
                           (count-leaves-without-map node))))) 
               0 
               t))


(assertEqual (count-leaves-without-map '()) 0)
(assertEqual (count-leaves-without-map (list 1)) 1)
(assertEqual (count-leaves-without-map (list 1 2 3)) 3)
(assertEqual (count-leaves-without-map (list (list 1 2) (list 1 2 3) 1)) 6)