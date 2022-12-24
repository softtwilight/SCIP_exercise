#lang racket
(require sicp-pict)
(define indentity (lambda (x) x))
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; exercise 2.45
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter
               (op2 smaller smaller))))))
   
         


(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four indentity flip-vert
                                  indentity flip-vert)))
    (combine4 painter)))
(paint (flipped-pairs einstein))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz indentity
                                  rotate180 flip-vert)))
    (if (= n 0)
        painter
        (combine4 (corner-split painter (- n 1))))))
(paint (square-limit einstein 2))