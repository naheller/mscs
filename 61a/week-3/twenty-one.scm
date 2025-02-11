#lang simply-scheme

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

; #1 Best total
(define (best-total hand)
  (define (bt hand aces total)
    (cond ((not (empty? hand))
	  (if (is-ace? (first hand))
	    (bt (bf hand) (se (first hand) aces) total)
	    (bt (bf hand) aces (+ (get-card-value (first hand)) total))))
	  ((not (empty? aces))
	  (if (<= (+ total 11) 21)
	    (bt hand (bf aces) (+ total 11))
	    (bt hand (bf aces) (+ total 1))))
	  (else total)))
  (bt hand '() 0))

(define (get-card-value card)
  (if (number? (first card))
    (first card)
    10))

(define (is-ace? card) (equal? (first card) 'a))

; #2 Strategy: Stop at 17
(define (stop-at-17 cust-hand-so-far dealer-up-card)
  (if (< (best-total cust-hand-so-far) 17)
    #t
    #f))

; #3 Play n
(define (play-n strategy num-games)
  (if (= num-games 0)
    0
    (+ (twenty-one strategy) (play-n strategy (- num-games 1)))))

; #4 Strategy: Dealer sensitive
(define (dealer-sensitive cust-hand-so-far dealer-up-card)
  (define SET_ONE '(a 7 8 9 10 j q k))
  (define SET_TWO '(2 3 4 5 6))

  (if (or
	(and (< (best-total cust-hand-so-far) 17)
	     (member? (first dealer-up-card) SET_ONE))
	(and (< (best-total cust-hand-so-far) 12)
	     (member? (first dealer-up-card) SET_TWO)))
    #t
    #f))
	
; #5 Stop at n
(define (stop-at n)
  (lambda (cust-hand-so-far dealer-up-card)
    (if (< (best-total cust-hand-so-far) n)
      #t
      #f)))

; #6 Valentine's
(define (valentine cust-hand-so-far dealer-up-card)
  (define STOP_VAL (if (has-heart? cust-hand-so-far) 19 17))
  (if (< (best-total cust-hand-so-far) STOP_VAL) #t #f))

(define (has-heart? hand)
  (cond ((empty? hand) #f)
	((equal? (bf (first hand)) 'h) #t)
	(else (has-heart? (bf hand)))))

; #7 Suit strat
(define (suit-strategy suit strat-has strat-hasnt)
  (lambda (cust-hand-so-far deal-up-card)
    (if (has-heart? cust-hand-so-far)
      (strat-has cust-hand-so-far deal-up-card)
      (strat-hasnt cust-hand-so-far deal-up-card))))

(define (has-suit? suit hand)
  (cond ((empty? hand) #f)
    ((equal? (bf (first hand)) suit) #t)
    (else (has-suit? suit (bf hand)))))

; Redefine Valentine
(define (valentine-2 cust-hand-so-far dealer-up-card)
  (define strategy (suit-strategy 'h (stop-at 19) (stop-at 17)))
  (strategy cust-hand-so-far dealer-up-card))

; #8 Majority
(define (majority s1 s2 s3)
  (define (bool-to-num bool)
    (if (equal? bool #t) 1 0))
  (lambda (hand up-card)
    (let ((r1 (s1 hand up-card))
	   (r2 (s2 hand up-card))
	   (r3 (s3 hand up-card)))
       (if (< (+
		(bool-to-num r1)
		(bool-to-num r2)
		(bool-to-num r3)
		) 2) #f #t))))
;                                      32
