#lang racket
(require sicp-pict)

(defne (make-segment v1 v2)
  (cons v1 v2))
(define start-segment car)
(define end-segment cdr)


(define segments->painter 1)

(define (outline frame)
  (segments->painter
   (list
    (segment (make-vect 0 0) (make-vect 0 1))
    (segment (make-vect 0 0) (make-vect 1 0))
    (segment (make-vect 0 1) (make-vect 1 1))
    (segment (make-vect 1 0) (make-vect 1 1)))))
    