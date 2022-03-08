 #lang sicp
(#%require "../../util/utils.rkt")

(define (for-each proc items)
  (if (null? items)
      #t
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 1 2 3))

