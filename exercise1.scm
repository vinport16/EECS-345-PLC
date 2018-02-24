; Vincent Portelli VLP25

; 1
; finds a number larger than n, inserts n in front of it
(define insert
	(lambda (n lis)
		(if (> (car lis) n)
			(cons n lis)
			(cons (car lis) (insert n (cdr lis))))))
(insert '3 '(0 1 2 4 5) )
; (0 1 2 3 4 5)

; 2
; compares the two first numbers in the lists, returns the smaller one plus the
; merge of the lists (minus the element that was taken out of one of the lists)
(define merge
	(lambda (lisa lisb)
		(cond
			((null? lisa) lisb)
			((null? lisb) lisa)
			((< (car lisa) (car lisb)) (cons (car lisa) (merge (cdr lisa) lisb)))
			(else (cons (car lisb) (merge lisa (cdr lisb))))
			)))
(merge '( 1 2 5 7) '( 4 7 9 9 66 81))
; (1 2 4 4 7 7 9 9 66 81)

; 3
; checks if the first two elements are equal. skips over one if they are. cons' to
; the removedups of the rest of the list
(define removedups
	(lambda (lis)
		(cond
			((null? (cdr lis)) lis)
			((equal? (car lis) (car (cdr lis)))  (removedups (cdr lis)) )
			(else (cons (car lis) (removedups (cdr lis))))
			)))

(removedups '( a b b c dd dd d e e a a a a ) )
; (a b c dd d e a)

; 4
; takes the first two elements splits them, adding them to the first and second list
; that result from split of (cons (cons lis)) respectively
(define split
	(lambda (lis)
		(cond
			((null? lis) (cons '() (cons '() '())) )
			((null? (cdr lis)) (cons (car lis) (cons '() '())) )
			(else (cons
								(cons
									(car lis)
									(car (split (cdr (cdr lis)))))
								(cons
									(cons
										(car (cdr lis))
										(car (cdr (split (cdr (cdr lis))))))
								'())
						))
			)))
(split '(0 1 2 3 4 5 6 7 8 9))
; ((0 2 4 6 8) (1 3 5 7 9))

; 5
; if the first element is a list, deepcons a to it. if it is not, cons a onto lis
(define deepcons
	(lambda (a lis)
		(cond
			((null? lis) (cons a lis))
			((list? (car lis)) (cons(deepcons a (car lis)) (cdr lis)))
			(else (cons a lis))
			)))
(deepcons 'x '((((((a b c d) e) f) g) h) i) )
; ((((((x a b c d) e) f) g) h) i)
(deepcons 'x '(() a b c) )
; ((x) a b c)

; 6
; every time the end of a list is found, return a 2. all of the twos add to the #
; of parenthases
(define numparens
	(lambda (lis)
		(cond
				((null? lis) 2)
				((list? (car lis)) (+ (numparens (car lis)) (numparens (cdr lis))))
				(else (numparens (cdr lis)))
				)))
(numparens '(()()(aa bb (c) dd)) )
; 10

; 7
; cons every element back on to the dup* of (cdr lis) twice. if the (car lis) is a list,
; then cons the dup* of it onto the dup* of (cdr lis) twice.
(define dup*
	(lambda (lis)
		(cond
			((null? lis) lis)
			((list? (car lis)) (cons (dup* (car lis)) (cons (dup* (car lis)) (dup* (cdr lis))) ))
			(else (cons (car lis) (cons (car lis) (dup* (cdr lis)))))
			)))
(dup* '(1 2 (3 4) 5))
; (1 1 2 2 (3 3 4 4) (3 3 4 4) 5 5)

; 8
; calls removedups* on all sublists before cons-ing them back in
(define removedups*
	(lambda (lis)
		(cond
			((null? (cdr lis))
				(if (list? (car lis))
					(cons (removedups* (car lis)) '())
					lis
					)
				)
			((list? (car lis)) (cons (removedups* (car lis)) (removedups* (cdr lis))))
			((equal? (car lis) (car (cdr lis)))  (removedups* (cdr lis)) )
			(else (cons (car lis) (removedups* (cdr lis))))
			)))
(removedups* '(a a (b b b (d d) b ((d) d)) f (f f g)))
; (a (b (d) b ((d) d)) f (f g))


; 9
; split but when a sublist is encountered, it is also split and stored in the place
; of the sublist
(define split*
	(lambda (lis)
		(cond
			((null? lis) (cons '() (cons '() '())) )
			((and (null? (cdr lis)) (list? (car lis)))
				(cons (cons (split* (car lis)) '()) (cons '() '() ))
				)
			((null? (cdr lis)) (cons (car lis) (cons '() '())) )
			(else (cons
								(cons
									(if (list? (car lis))
										(split* (car lis))
										(car lis)
										)
									(car (split* (cdr (cdr lis)))))
								(cons
									(cons
										(if (list? (car(cdr lis)))
											(split* (car (cdr lis)))
											(car (cdr lis))
											)
										(car (cdr (split* (cdr (cdr lis)))))
										)
								'())
						))
			)))
(split* '(a b ((c d) e f g) (((h i) j k l (m n o p)))))
;((a ((((c) (d)) f) (e g))) (b ((((((h) (i)) k ((m o) (n p))) (j l))) ())))


; 10
; calls removedupes on all sublists before comparing them with equal?, removing one
; if they are the same
(define removedups**
	(lambda (lis)
		(cond
			((null? lis)
				lis)
			((null? (cdr lis))
				(if (list? (car lis))
					(cons (removedups** (car lis)) '())
					lis
					))
			((not (list? (car lis)))
				(if (list? (car (cdr lis)))
					(cons (car lis) (removedups** (cdr lis)))
					(if (equal? (car lis) (car (cdr lis)))
						(removedups** (cdr lis))
						(cons (car lis) (removedups** (cdr lis)))
						)
					)
				)
			(else
				(if (list? (car (cdr lis)))
					(if (equal? (removedups** (car lis)) (removedups** (car (cdr lis))))
						(cons (removedups** (car lis)) (removedups** (cdr (cdr lis))))
						(cons (removedups** (car lis)) (removedups** (cdr lis)))
						)
					(cons (removedups** (car lis)) (removedups** (cdr lis)))
					)
				)
			)
		)
	)
(removedups** '(a a (b b b (d d) b ((d) d)) f (f f g)))
; (a (b (d) b ((d) d)) f (f g))
(removedups** '( (a b c) (a a b c c) a q (q) (q u (a)) (q u (a a) (a))))
; ((a b c) a q (q) (q u (a)))
















