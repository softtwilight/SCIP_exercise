 #lang sicp
(#%require "../../util/utils.rkt")

(define (last-pair items)
  (define (iter a b)
    (if (null? b)
        a
        (iter b (cdr b))))
  (iter items (cdr items)))

(assertEqual 9 (car (last-pair (list 2 3 5 9))))
(assertEqual 1 (length (last-pair (list 2 3 5 9))))
