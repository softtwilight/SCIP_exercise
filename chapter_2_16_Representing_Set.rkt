#lang sicp

;; exercise=2.59
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define set1 (list 1 2 3))
(define set2 (list 2 3 4))
(intersection-set set1 set2)
(union-set set1 set2)


;; Sets as ordered lists
;; exercise_2.61
(define (element-of-order-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


;; average n/2 operations 
(define (adjoin-order-set x set)
  (define (inner acc rest)
    (cond ((null? rest) (append acc (list x)))
          ((= x (car rest)) set)
          ((< x (car rest)) (append (append acc (list x)) rest))
          (else (inner (append acc (list (car rest))) (cdr rest)))))
  (inner '() set))
(adjoin-order-set 6 (list 1 2 3 4 5)) ;; -> (1 2 3 4 5 6)

(define (union-order-set set1 set2)
  (define (inner result rest1 rest2)
    (cond ((null? rest1) (append result rest2))
          ((null? rest2) (append result rest1))
          ((= (car rest1) (car rest2))
           (inner (append result (list (car rest1)))
                  (cdr rest1) (cdr rest2)))
          ((< (car rest1) (car rest2))
           (inner (append result (list (car rest1)))
                  (cdr rest1) rest2))
          (else (inner (append result (list (car rest2)))
                       rest1 (cdr rest2)))))
  (inner '() set1 set2))

(union-order-set set1 set2) ;; -> (1 2 3 4)                   

;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-tree-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree-set? x (right-branch set)))))

(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-tree-set x (right-branch set))))))

;; exercise_2.63
;; partial-tree splits the list into three part, entry, left tree, right tree
;; the entry is the media point, left tree is left part of ordered list, recursice
;; call partial-tree, the right tree is same, the combine a final tree plus
;; remain elements

;; T(n) = 2T(n ÷ 2) + Θ(1)
;; By the Master Theorem, we have a = 2, b = 2, and f(n) = Θ(1).
;; Therefore, T(n) = Θ(n).

;; exercise_2.64 略
;; exercise_2.65
;; use 2.63 to convert tree to sorted list
;; then use union-order-set and intersection-order-set to output a result sorted list
;; then use list->tree method to construct a balance tree
;; every single method is Θ(n), so the result is Θ(n).

;; exercise_2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        ((small? given-key (key (entry set-of-records)))
         (lookup given-key (left-tree set-of-records)))
        (else (lookup given-key (right-tree set-of-records)))))

