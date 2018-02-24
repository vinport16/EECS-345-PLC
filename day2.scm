; create a list with n copies of x
(define repeat
	(lambda (n x)
		(if (zero? n)
			'()
			(cons x (repeat (- n 1) x)))))

; remove the first instance of x in the list
(define removefirst
	(lambda (x lis)
		(cond
			((null? lis) lis)
			((eq? x (car lis)) (cdr lis))
			(else (cons (car lis) (removefirst x (cdr lis)))))))

; replace the first instance of x with y
(define replacefirst
	(lambda (x y lis)
		(cond
			((null? lis) lis)
			((eq? x (car lis)) (cons y (cdr lis)))
			(else (cons (car lis) (removefirst x (cdr lis)))))))

; remove all x from lis
(define removeall
	(lambda (x lis)
		(cond
			((null? lis) lis)
			((eq? x (car lis)) (removeall x (cdr lis)))
			(else (cons (car lis) (removeall x (cdr lis)))))))

; squares: return a list of the squares of the elements of lis
(define squares
	(lambda (lis)
		(cond
			((null? lis) lis)
			(else (cons (* (car lis) (car lis)) (squares (cdr lis)))))))

; concatanate two lists
(define myappend
	(lambda (lisa lisb)
		(if (null? lisa)
			lisb
			(cons (car lisa) (myappend (cdr lisa) lisb)))))

; reverse lis   ############ broken
(define myreverse
	(lambda (lis)
		(if (null? lis)
			lis
			(myappend (myreverse (cdr lis)) (cons (car lis) '()) ))))

; apply a function to every element in a list
(define mymap
	(lambda (f lis)
		(if (null? lis)
			lis
			(cons (f(car lis)) (mymap (cdr lis) f)))))

; transpose (turn matrix 90º clockwise)  ##### uhhhhh check this again
(define transpose
	(lambda (m)
		(if (null? (car m))
			m
			(cons (mymap car m) (transpose (mymap (cdr m)))))))

(transpose '((10 20 30) (11 21 31) (12 32 33)))












