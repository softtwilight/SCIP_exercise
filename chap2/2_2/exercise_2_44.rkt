#lang racket
(require sicp-pict)
;;(paint einstein)
;;(paint (beside einstein einstein))
;;(paint
;;(below
;;  (flip-vert (beside einstein einstein))
;;  (beside einstein einstein)
;;  ))

;;(paint (beside einstein (flip-horiz einstein)))
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;;(paint (flipped-pairs einstein))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;;(paint (right-split einstein 4))

;; exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

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
;; interesting!
(paint (corner-split einstein 4))

;;(paint diagonal-shading)

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;; amazing
(paint (square-limit einstein 4))


