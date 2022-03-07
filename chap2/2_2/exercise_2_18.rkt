 #lang sicp
(#%require "../../util/utils.rkt")

(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items))
            (list (car items)))))


(define result (reverse (list 2 3 5 9)))
(assertEqual 9 (car result))
(assertEqual 5 (list-ref result 1))
(assertEqual 3 (list-ref result 2))
(assertEqual 2 (list-ref result 3))
(assertEqual 4 (length result))

