(define factorial-cps
	(lambda (n exit)
		(if (eq? n 1)
			(exit n)
			(factorial-cps (- n 1) (lambda (v) (exit (* n v)))))))
(define factorial
	(lambda (n)
		(factorial-cps n (lambda (v) v))))