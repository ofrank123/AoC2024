#lang racket
(define input (file->string "inputs/day01.txt"))

(define (odds lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) (list (car lst)))
          (else (cons (car lst) (odds (cddr lst))))))

(define (unzip-and-map fn lst)
  (list (fn (odds lst)) (fn (odds (cdr lst)))))

(define lsts (unzip-and-map (lambda (lst) (sort lst <))
               (map string->number (regexp-match* "[0-9]+" input))))

(foldl (lambda (a b result)
         (+ result (abs (- a b))))
       0
       (first lsts)
       (second lsts))