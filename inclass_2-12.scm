;tail recursion

;accumulator passing style
;length of a list
(define len
	(lambda (lis)
		(if (null? lis)
			0
			(+ 1 (len (cdr lis))))))
; better
(define len-acc
	(lambda (lis acc)
		(if (null? lis)
			acc
			(len-acc (cdr lis) (+ 1 acc)))))

;sum the numbers in a list
(define sumbers
	(lambda (lis)
		(cond
			((null? lis) 0)
			((number? (car lis)) (+ (car lis) (sumbers (cdr lis))))
			(else (sumbers (cdr lis)))
			)))
;better  (not finished, doesn't work)
(define sumbers-acc
	(lambda (lis acc)
		(cond
			((null? lis) 0)
			((number? (car lis)) (+ (car lis) (sumbers (cdr lis))))
			(else (sumbers (cdr lis)))
			)))


; continuation passing style
(define sumbers-cps
	(lambda (lis return)
		(cond
			((null? lis) (return 0))
			((number? (car lis)) (sumbers-cps (cdr lis) (lambda (v) (return (+ (car lis) v)))))
			(else (sumbers-cps (cdr lis) return)))))

(sumbers-cps '(a 1 2 3 4 5 d g 0 ) (lambda (v) v) ) ; => 15

(define factorial-cps
	(lambda (n return)
		(if (zero? n)
			(return 1)
			(factorial-cps (- n 1) (lambda (v) (return (* n v)))))))
(factorial-cps 4 (lambda (v) v)) ; => 24


; exercises

;myappend (cps)
(define myappend
	(lambda (lisa lisb return)
		(cond
			((null? lisa) (return lisb))
			(else (myappend (cdr lisa) lisb (lambda (v) (return  (cons (car lisa) v)))))
			)))
(myappend '(a b c d) '(e f g h) (lambda (v) v) )

;squares (cps)
(define squares
	(lambda (lis return)
		(cond
			((null? lis) (return lis))
			(else (squares (cdr lis) (lambda (v) (return (cons (* (car lis) (car lis)) v))) ))
			)))
(squares '(1 2 3 7 33 2 2 5) (lambda (v) v) )

;removeall (cps)
(define )

;reverse (cps)
(define reverse
	(lambda (lis return)
		(cond
			((null? lis) (return lis))
			(else (reverse (cdr lis) (lambda (v)   (myappend v (cons (car lis) '() )     (lambda (v) (return v)) ))))
			)))
(reverse '(1 2 3 7 33 2 2 5) (lambda (v) v) )






















