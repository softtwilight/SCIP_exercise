 #lang sicp
(#%require "../../util/utils.rkt")

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))


(define (assertListEqual l1 l2)
  (if (null? l1)
      (if (null? l2)
          (print "success")
          (print "length different"))
      (if (= (car l1) (car l2))
          (assertListEqual (cdr l1) (cdr l2))
          (print "item not equal"))))


(assertListEqual (list 1 4 9 16) (square-list (list 1 2 3 4)))
(assertListEqual (list 1 4 9 16) (square-list-2 (list 1 2 3 4)))


