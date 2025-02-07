#lang simply-scheme

(define (inc x) (+ x 1))

(define (sq x) (* x x))

(define (every f sent)
  (if (= (count sent) 1)
    (f (first sent))
    (se (f (first sent)) (every f (bf sent)))))
