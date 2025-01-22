#lang simply-scheme
;(define (plural wd)
;  (word wd 's))

(define (plural wd)
  (if (and (equal? (last wd) 'y) (is-cons? (last (bl wd))))
      (word (bl wd) 'ies)
      (word wd 's)))

(define (is-cons? letter)
  (not (member letter `(a e i o u y))))
