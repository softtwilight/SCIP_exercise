 #lang sicp
(#%require "../../util/utils.rkt")

(define (same-parity first . items)
  (define testor 
    (if (even? first)
        even?
        odd?))
  (define (helper items)
    (if (null? items)
        '()
        (if (testor (car items))
            (cons (car items)
                  (helper (cdr items)))
            (helper (cdr items)))))
  (cons first
        (helper items)))


(define (assertListEqual l1 l2)
  (if (null? l1)
      (if (null? l2)
          (print "success")
          (print "length different"))
      (if (= (car l1) (car l2))
          (assertListEqual (cdr l1) (cdr l2))
          (print "error"))))

(assertListEqual (list 1 3 5 7) (same-parity 1 2 3 4 5 6 7))
(assertListEqual (list 2 4 6) (same-parity 2 3 4 5 6 7))

