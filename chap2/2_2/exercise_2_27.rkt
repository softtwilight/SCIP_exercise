 #lang sicp
(#%require "../../util/utils.rkt")

(define x (list (list 1 2) (list 3 4) (list 5 (list 7 8))))


(define (deep-reverse x)
  (cond ((empty-tree? x) nil)
        ((leaf-node? x) x)
        (else (append (deep-reverse (right-branch x))
                    (list (deep-reverse (left-branch x)))))))

(define empty-tree? null?)
(define leaf-node? (compose pair? not))
(define left-branch car)
(define right-branch cdr)

;; (((8 7) 5) (4 3) (2 1))
(deep-reverse x)
;; (10 (9 (8 7) 6) 5 (4 3) 2 1)
(deep-reverse '(1 2 (3 4) 5 (6 (7 8) 9) 10)) 
        