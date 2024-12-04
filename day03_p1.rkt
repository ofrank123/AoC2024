#lang racket
(define input (file->string "inputs/day03.txt"))

(foldl + 0 (map (lambda (ins) (* (first ins) (second ins)))
                (map (lambda (ins)
                       (map string->number
                            (regexp-match* "[0-9]+" ins)))
                     (regexp-match* "mul\\([0-9]+,[0-9]+\\)" input))))