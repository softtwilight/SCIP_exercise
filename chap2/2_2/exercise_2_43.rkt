#lang racket

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position
           new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

;; louis method
;; (queen-cols) is a recursive call, in the first version, it call once
;; T(k) = T(k - 1) + n * S(k - 1), T is complixty, S is size of queen-cols at k. 
;; in above version, it call n times
;; T(k) = n * T(k - 1) + n * S(k - 1).
;; queen-cols is expensive than (adjoin-position) operation.