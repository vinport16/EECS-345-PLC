(define foldleft
	(lambda (f i l)
		(cond
			((null? l) i )
			(else (foldleft f (f i (car l)) (cdr l)))
			)))

(foldleft - 10 '(1 2 3))

(define foldright
	(lambda (f i l)
		(cond
			((null? l) i)
			(else (f (car l) (foldright f i (cdr l)) ))
			)))

(foldright - 10 '(1 2 3))

(define mymap
	(lambda (f l)
		(if (null? l)
			l
			(cons (f (car l)) (mymap f (cdr l))))
			))

(mymap (lambda (x) (+ x 100)) '(1 2 3 4 5 6))

(define filter
	(lambda (f l)
		(cond
			((null? l) l)
			((f (car l)) (cons (car l) (filter f (cdr l))))
			(else (filter f (cdr l)))
			)))

(filter (lambda (x) (eq? x 5)) '(1 2 5 3 4 5 6))

(define flip
	(lambda (a b c) (c b a)))

(define applyit
	(lambda (f lisa lisb)
		(cond
			((null? lisa) lisb)
			((null? lisb) lisa)
			(else (cons (f (car lisa) (car lisb)) (applyit f (cdr lisa) (cdr lisb))))
			)))

(applyit + '(1 2 3 4 5) '(10 20 30 40 50))

(define dotproduct ; doesn't work
	(lambda (lisa lisb)
		(foldleft + 0 (applyit * lisa lisb))))

(dotproduct '((1 2 3)(2 3 4)(3 4 5)) '((7 6 5)(6 5 4)(5 4 3)))

(define qsort
	(lambda (lis)
		(if (null? lis)
			lis
			(qsort (filter (lambda (x) (x < h) (cdr lis) ) ))))) ; not done ?????




















