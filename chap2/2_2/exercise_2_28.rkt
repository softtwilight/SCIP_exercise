 #lang sicp
(#%require "../../util/utils.rkt")

(define x (list (list 1 2) (list 3 4) (list 5 (list 7 8))))


(define (fringe x)
  (cond ((empty-tree? x) nil)
        ((leaf-node? x) (list x))
        (else (append (fringe (left-branch x))
                      (fringe (right-branch x))))))

(define empty-tree? null?)
(define leaf-node? (compose pair? not))
(define left-branch car)
(define right-branch cdr)

;; (1 2 3 4 5 7 8)
(fringe x)
;; (10 9 8 7 6 5 4 3 2 1)
(fringe '(10 (9 (8 7) 6) 5 (4 3) 2 1)) 
        