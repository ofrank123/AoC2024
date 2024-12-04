#lang racket
(define input (file->lines "inputs/day04.txt"))

(define grid
  (let ([list->ivec (λ (lst) (vector->immutable-vector (list->vector lst)))])
    (list->ivec (map list->ivec (map string->list input)))))

(define width (vector-length (vector-ref grid 0)))
(define height (vector-length grid))
(define (get p)
  (vector-ref (vector-ref grid (cadr p)) (car p)))

(define ns (list '(-1 1) '(0 1) '(1 1) '(-1 0) '(1 0) '(-1 -1) '(0 -1) '(1 -1)))
(define stens
  (map
   (λ (l)
     (map (λ (a b) (list (* (car a) b) (* (cadr a) b)))
          (make-list 3 l)
          '(1 2 3)))
   ns))

(define (check-pos pc)
  (let ([vstens (filter
                 (λ (ps) (andmap
                          (λ (p)
                            (let ([px (car p)]
                                  [py (cadr p)])
                              (and (>= px 0) (< px width) (>= py 0) (< py height))))
                          ps))
                 (map (λ (ss) (map (λ (p) (map + p pc)) ss)) stens))])
    (length (filter (λ (word) (andmap eq? word '(#\X #\M #\A #\S)))
            (map (λ (vsten) (cons (get pc) (map get vsten))) vstens)))))


(define word-cts
  (map (λ (a b) (map (λ (x y) (check-pos (list x y))) a b))
       (make-list height (range 0 width))
       (map (λ (a) (make-list width a)) (range 0 height))))

(foldl + 0 (map (λ (lst) (foldl + 0 lst)) word-cts))


     