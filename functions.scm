;return length of list len
(define len
	(lambda (lis)
		(if (null? lis)
			0
			(+ 1 (len (cdr lis))))))

; returns true if the element is in the list lis
(define member?
	(lambda (a lis)
		(if (null? lis)
			#f
			(if (eq? a (car lis))
				#t
				(member? a (cdr lis))))))

; member? using cond
(define member?
	(lambda (a lis)
		(cond
			((null? lis) #f)
			((eq? a (car lis)) #t)
			(else (member? a (cdr lis))))))

; factorial
(define factorial
	(lambda (n)
		(if (eq? n 1)
			1
			(* n (factorial (- n 1))))))


; count nums
(define countnums
	(lambda (lis)
		(if (null? lis)
		0
		(if (number? (car lis))
			(+ 1 (countnums (cdr lis)))
			(countnums (cdr lis)) ))))

; sum nums
(define sumnums
	(lambda (lis)
		(if (null? lis)
		0
		(if (number? (car lis))
			(+ (car lis) (sumnums (cdr lis)))
			(sumnums (cdr lis)) ))))