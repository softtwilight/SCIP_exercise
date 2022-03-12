 #lang sicp
(#%require "../../util/utils.rkt")

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map
                 (lambda (x)
                   (if (null? x)
                       (list (car s))
                   (cons (car s) x)))
                 rest)))))

(subsets (list 1 2 3))