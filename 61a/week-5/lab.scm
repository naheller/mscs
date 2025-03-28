#lang sicp

; Ex 2.25 - Select 7
(define l (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l)))))

(define l2 (list (list 7)))
(car (car l2))

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; Ex 2.53 - What does intepreter print?
(list 'a 'b 'c)
; Guess: ('a 'b 'c)
; Actual: (a b c)

(list (list 'george))
; Guess: (('george))
; Actual ((george))

(cdr '((x1 x2) (y1 y2)))
; Guess: (y1 y2)
; Actual: ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; Guess: (y1)
; Actual (y1 y2)