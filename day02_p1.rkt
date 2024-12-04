#lang racket
(define input (file->lines "inputs/day02.txt"))

(define reports
  (map
   (lambda (lst)
     (map string->number (regexp-match* "[0-9]+" lst)))
   input))

(define (null-or-single? lst) (or (null? lst) (null? (cdr lst))))

(define (check-report-cmp cmp r)
  (cond ((null-or-single? r) #t)
        (else (let ([a (first r)]
                    [b (second r)])
                (and (and (cmp a b) (<= (abs (- a b)) 3))
                     (check-report-cmp cmp (cdr r)))))))

(define (check-report r)
  (cond ((> (first r) (second r)) (check-report-cmp > r))
        (else (check-report-cmp < r))))

(foldl (lambda (r res) (+ res (if (check-report r) 1 0)))
       0
       reports)