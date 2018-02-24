; removeall* removes atom x from lis where lis can contain sublists
; (removeall* 'x '(a x () (l o l x o l) c (e e) x t) ) => (a () (l o l o l) c (e e) t)

(define removeall*
	(lambda (x lis)
		(cond
			((null? lis)
				lis)
			((list? (car lis))
				(cons (removeall* x (car lis)) (removeall* x (cdr lis))))
			((eq? x (car lis))
				(removeall* x (cdr lis)))
			(else
				(cons (car lis) (removeall* x (cdr lis)))))))

; member*? => #t if x is anywhere in lis (any level of lis)

(define member*?
	(lambda (x lis)
		(cond
			((null? lis) #f)
			((pair? (car lis)) (or (member*? x (car lis)) (member*? x (cdr lis))))
			((eq? x (car lis)) #t)
			(else (member*? x (cdr lis))))))

(member*? 'x '(a b s e ( c ( x) ( 0 ) ) ) )

;replaceall* 'x 'y lis
(define replaceall*
	(lambda (x y lis)
		(cond
			((null? lis) lis)
			((list? (car lis)) (cons (replaceall* x y (car lis)) (replaceall* x y (cdr lis))))
			((eq? x) (cons y (replaceall* x y (cdr lis))))
			(else (cons (car lis) (replaceall* x y (cdr lis)))))))

;empty* (take all the atoms out of the lists)
(define empty*
	(lambda (lis)
		(cond
			((null? lis) lis)
			((list? (car lis)) (cons (empty* (car lis)) (empty* (cdr lis))))
			(else (empty* (cdr lis))))))

;flatten* (makes it a one level list)
(define flatten*
	(lambda (lis)
		(cond
			((null? lis) lis)
			((list? (car lis)) (append (flatten* (car lis)) (flatten* (cdr lis)) ))
			(else (cons (car lis) (flatten* (cdr lis))) ) )))

















