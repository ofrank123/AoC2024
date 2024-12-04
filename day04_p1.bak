#lang racket
(define input (file->string "inputs/day03.txt"))

(define (exec-insts e insts)
  (if (null? insts) 0
      (let ([inst (car insts)])
        (cond ((boolean? inst) (exec-insts inst (cdr insts)))
              (else (+ (if e inst 0)
                       (exec-insts e (cdr insts))))))))

(exec-insts
 #t
 (map (λ (inst)
        (cond ((string=? inst "do()") #t)
              ((string=? inst "don't()") #f)
              (else ((λ (p) (* (first p) (second p)))
                     (map string->number
                          (regexp-match* "[0-9]+" inst))))))
      (regexp-match* "don't\\(\\)|do\\(\\)|mul\\([0-9]+,[0-9]+\\)" input)))
