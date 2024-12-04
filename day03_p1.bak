#lang racket
(define input (file->lines "inputs/day02.txt"))

(define reports
  (map
   (lambda (lst)
     (map string->number (regexp-match* "[0-9]+" lst)))
   input))

(define (null-or-single? lst) (or (null? lst) (null? (cdr lst))))

(define (check-report-cmp cmp bad r)
  (cond ((null-or-single? r) #t)
        (else (let ([a (first r)]
                    [b (second r)])
                (or (and (and (cmp a b) (<= (abs (- a b)) 3))
                         (check-report-cmp cmp bad (cdr r)))
                    (and (not bad)
                         (check-report-cmp cmp #t (cons (car r) (cddr r)))))))))

(define (check-report r)
  (or (check-report-cmp (if (> (first r) (second r)) > <) #f r)
      (check-report-cmp (if (> (second r) (third r)) > <) #t (cdr r))
      (check-report-cmp (if (> (first r) (third r)) > <) #t (cons (car r) (cddr r)))))

(foldl (lambda (r res) (+ res (if (check-report r) 1 0)))
       0
       reports)