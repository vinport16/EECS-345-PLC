(define multiply-cps
	(lambda (lis return break)
		(cond
			((null? lis) (return 1))
			((zero? (car lis)) (break 0))
			(else (multiply-cps (cdr lis) (lambda (v) (return (* (cdr lis) v))) break ))
			)))

(multiply-cps '(3 4 5 5) (lambda (v) v) (lambda (x) x) )

(define call/cc call-with-current-continuation)

(define multiply
	(lambda (lis break)
		(cond
			((null? lis) 1)
			((zero? (car lis)) (break 0))
			(else (* (car lis) (multiply (cdr lis) break)))
			)))
(mulitply '( 1 2 3 4) (lambda (v) v))

(+ 10 (call/cc (lambda (break) (* 5 (break 3))))) ; => 13
(+ 10 (call/cc (lambda (break) (* 5 (break (+ (break 2) (break 3))))))) ; => 12

(define mycont (lambda (v) v))

; do call/cc outside of recursion
(define replaceall
	(lambda (x y lis)
		(call/cc
			(lambda (break)
				(cond
					((null? lis) (break (set! mycont break) lis))
					((eq? x (car lis)) (cons y (replaceall x y (cdr lis))))
					(else (cons (car lis) (replaceall x y (cdr lis)) ))
					)))))

(replaceall 'x 'y '(a x b x c x d))

; this is bad code (replaceall and appendit combined)
(define appendit
	(lambda (lisa lisb)
		(if (null? lisa)
			(mycont lisb)
			(cons (car lisa) (appendit (cdr lisa) lisb)))))
(appendit '(a b c) '(d e f))

(replaceall 'x 'y '(this is craz x))

; ok new code here
; goodmult
(define goodmult
	(lambda (lis)
		(call/cc
			(lambda (break)
				(multiply lis break)))))

;;;; (indexof 'x '(a b x c d e f) ) => 2 (or -1 if not in list)
(define indexof
	(lambda (x lis)
		(cond
			((null? lis) ???)
			((eq? x (car lis)) 0)
			(else (+ 1 (indexof x (cdr lis))))
			)))

(define indexof
	(lambda (x lis)
		))















