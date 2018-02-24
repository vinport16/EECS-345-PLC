
; continuation passing style

(define myappend ; this one is normal
	(lambda (lisa lisb)
		(if (null? lisa)
			lisb
			(cons (car l1) (myappend (cdr lisa) lisb)))))

(define myappend ; this one is cps
	(lambda (lisa lisb return)
		(cond
			((null? lisa) (return lisb))
			(else (myappend (cdr lisa) lisb (lambda (v) (return  (cons (car lisa) v)))))
			)))
(myappend '(a b c d) '(e f g h) (lambda (v) v) ) ; => (a b c d e f g h)

;replace x with y in a list (not sublists)  (doens't wokr)
(define replace
	(lambda (lis x y return)
		(cond
			((null? lis) (return lis))

			((eq? (car lis) x) (replace lis x y (lambda (v) (return (cons y v))) )
			(else              (replace lis x y (lambda (v) (return (cons (car lis) v))) )

			)))))
(replace '(j a z z x _ a n d _ x o l o) 'x 'y (lambda (v) v) )

;myreverse
(define myreverse
	(lambda (lis)
		(if (null? lis)
			'()
			(myappend (myreverse (cdr lis)) (cons (car lis) '() )))) ) 



;sumbers*: sum all numbers in list/sublists (doesn't work?)

(define sumbers*
	(lambda (lis return)
		(cond
			((null? lis) (return 0))
			((list? (car lis)) (sumbers* (car lis) (lambda (v1) (sumbers* (cdr lis) (lambda (v2) (return (+ v1 v2)))))))
			((number? (car lis)) (sumbers* (cdr lis) lambda (v) (return (+ (car lis) v))))
			(else (sumbers* (cdr lis) return))
			)))
(sumbers* '(1 1 2 (1 1 2) ((0) 1 2 2) 8) (lambda (v) v) )


;split-cps   (((((idk this doens't work)))))
(define split-cps
	(lambda (lis return)
		(cond
			((null? lis) (return '() '() ))
			((null? (cdr lis)) (return lis '()))
			(else (split-cps (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2)))))
			)))
(split-cps '(a b c d e f g) (lambda (v1 v2) (append v1 v2)))

;replaeall*-cps
(define replace
	(lambda (lis x y return)
		(cond
			((null? lis) (return lis))
			((list? (car lis)) (replace (car lis) x y (lambda (v) (replace (cdr lis) x y (lambda (w) (return (cons v w)))) )))
			((eq? (car lis) x) (replace (cdr lis) x y (lambda (v) (return (cons y v))) ))
			(else              (replace (cdr lis) x y (lambda (v) (return (cons (car lis) v))) ))
			)))
(replace '(j a z z x (a n d x) x o l o) 'x 'y (lambda (v) v) ) ; => (j a z z y (a n d y) y o l o)

;(flatten '(a b ((c d) e) ((f g)))) => '(a b c d e f g)
(define flatten
	(lambda (lis return)
		(cond
			((null? lis) (return lis))
			((list? (car lis)) (flatten (car lis) (lambda (v1) (flatten (cdr lis) (lambda (v2) (myappend v1 v2 return))))))
			(else (flatten (cdr lis) (lambda (v) (return (cons (car lis) v)))))
			)))

















