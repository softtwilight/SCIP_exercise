#lang sicp
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (remove x s)
  (filter (lambda (y) (not (= y x)))
          s))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (y) (cons x y))
                      (permutations (remove x s))))
               s)))
(permutations (list 1 2 3))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; exercise_2.40
(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(unique-pairs 5)

; (define (prime-sum-pairs n)
;   (map make-pair-sum
;       (filter prime-sum? (unique-pairs n))))

;; exercise_2.41
;; the power of high order function
;; really really beauty
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list k j i))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (sum list)
  (accumulate + 0 list))

(define (triples-small-than n s)
  (filter (lambda (tri)
            (<= (sum tri) s))
          (unique-triples n)))
(triples-small-than 5 7) ;; -> ((1 2 3) (1 2 4))

;; erercise_2.42

(define (safe? k position)
    (iter-check (car position) 
                (cdr position)
                 1))

(define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens) 
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
            (if (or (= row-of-new-queen row-of-current-queen) ;; is same col          
                    (= row-of-new-queen (+ i row-of-current-queen)) ;; is on ↗ line  
                    (= row-of-new-queen (- row-of-current-queen i))) ;; is on ↖ line
                #f
                (iter-check row-of-new-queen 
                            (cdr rest-of-queens)    
                            (+ i 1))))))            

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
      (cons new-row rest-of-queens))

  
(define (queens board-size) 
  (define (queen-cols k)  
    (if (= k 0) 
        (list empty-board) 
        (filter 
         (lambda (positions) (safe? k positions)) 
         (flatmap 
          (lambda (rest-of-queens) 
            (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))))) 
  (queen-cols board-size))
(queens 4) ;; -> ((3 1 4 2) (2 4 1 3))
                   
 ;; exercise_2.43
(flatmap 
 (lambda (new-row) 
   (map (lambda (rest-of-queens) 
          (adjoin-position new-row k rest-of-queens)) 
        (queen-cols (- k 1)))) 
 (enumerate-interval 1 board-size))
;; the new version evaluate (queen-col) n times every layer
;; the recursive depth is (n - 1) , so n * n * n ... n (n-1 n multiple)
;; n^(n-1) where n = 8