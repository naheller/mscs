#lang simply-scheme

(define (even? num) (= (remainder num 2) 0))

(define (square num) (* num num))

(define (fast-exp-rec b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-exp-rec b (/ n 2))))
	(else (* b (fast-exp-rec b (- n 1))))))

(define (fast-exp-iter-linear a b n)
  (if (= n 0) a
  (fast-exp-iter-linear (* a b) b (- n 1))))

; Appears to work correctly, but the body of cond case (= n 2)
; violates problem constraint whereby args passed to iter must
; always equate to invariant quantity a(b^n).
(define (fast-exp-iter-log b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((= n 2) (iter
		     (* a b)
		     (square b)
		     (- n 1)))
	  ((even? n) (iter
		       (* a b)
		       b
		       (/ n 2)))
	  (else (iter
		  (* a b)
		  b
		  (- n 1)))))
  (iter 1 b n))

; Correct solution from teacher notes
(define (fast-exp b n)
  (define (iter a b n)
    (cond ((= n 0) a)
	  ((even? n) (iter
		      a
		      (square b)
		      (/ n 2)))
	  (else (iter
		  (* a b)
		  b
		  (- n 1)))))
  (iter 1 b n))

; #2 Perfect Number
(define (divides-evenly? x y)
  (= (remainder x y) 0))

(define (sum-of-factors num)
  (define (sum potential-factor)
    (cond ((= potential-factor 0) 0)
	  ((divides-evenly? num potential-factor)
	   (+ potential-factor (sum (- potential-factor 1))))
	  (else (sum (- potential-factor 1)))))
  (sum (- num 1)))

(define (next-perf num)
  (if (= num (sum-of-factors num)) num
    (next-perf (+ num 1))))

; #3 Count Change
; Copied from text

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination
			  kinds-of-coins))
		     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
