#lang racket
(define (lookup given-key tree-records)
  (cond ((null? tree-records) #f)
        ((= given-key (key (entry tree-records)))
         (entry tree-records))
        ((< given-key (key (entry tree-records)))
         (lookup given-key (left-branch tree-records)))
        (else (lookup given-key (right-branch tree-records)))))