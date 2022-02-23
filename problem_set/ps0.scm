  #lang sicp
;(#%require "../util/utils.rkt")

; exersice 3

(define fact
  (lambda (n)
    (if (= n 1)
        1
	(* n (fact (- n 1))))))

(define (comb n k)
  (/ (fact n)
     (* (fact k)
        (fact (- n k)))))
(comb 243 90)
; answer = 193404342391239489855973693417880600543891038618846567058277413638164
