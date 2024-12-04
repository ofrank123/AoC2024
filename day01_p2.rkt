#lang racket
(define input (file->string "inputs/day01.txt"))

(define (odds lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) (list (car lst)))
          (else (cons (car lst) (odds (cddr lst))))))

(define (unzip lst)
  (list (odds lst) (odds (cdr lst))))

(define lsts (unzip
               (map string->number
                    (regexp-match* "[0-9]+" input))))

(foldl (lambda (a res)
         (+ res (* a (length (filter (lambda (n) (eq? n a)) (second lsts))))))
       0
       (first lsts))