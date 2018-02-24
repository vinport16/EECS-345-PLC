; (cadr x) = (car (cdr x))
; (caddr x) = (car (cdr (cdr x)))
; (caadr x) = (car (car (cdr x)))
; (cddr x) = (cdr (cdr x))

; m_value function: interprets prefix expressions
; '(* 3 5) => 15
; '(+ (- 2 1) 5) => 6

(define m_value
	(lambda (exp)
		(cond
			((number? exp)  exp)
			((eq? (car exp) '+)  (+ (m_value (cadr exp)) (m_value (caddr exp))))
			((eq? (car exp) '-)  (- (m_value (cadr exp)) (m_value (caddr exp))))
			((eq? (car exp) '*)  (* (m_value (cadr exp)) (m_value (caddr exp))))
			((eq? (car exp) '/)  (floor->exact (/ (m_value (cadr exp)) (m_value (caddr exp)))))
			((eq? (car exp) '%)  (- (m_value (cadr exp)) (* (floor->exact (/ (m_value (cadr exp)) (m_value (caddr exp)))) (caddr exp)) ))
			(else '(error undefined "bad op"))
			)))

;postfix kind
(define m_value
	(lambda (exp)
		(cond
			((number? exp)  exp)
			((eq? (caddr exp) '+)  (+ (m_value (car exp)) (m_value (cadr exp))))
			((eq? (caddr exp) '-)  (- (m_value (car exp)) (m_value (cadr exp))))
			((eq? (caddr exp) '*)  (* (m_value (car exp)) (m_value (cadr exp))))
			((eq? (caddr exp) '/)  (floor->exact (/ (m_value (car exp)) (m_value (cadr exp)))))
			((eq? (caddr exp) '%)  (- (m_value (car exp)) (* (floor->exact (/ (m_value (car exp)) (m_value (cadr exp)))) (cadr exp)) ))
			(else '(error undefined "bad op"))
			)))

;new thing: abstraction to put it in infix real quick
(define m_value
	(lambda (exp)
		(cond
			((number? exp)  exp)
			((eq? (operator exp) '+)  (+ (m_value (op1 exp)) (m_value (op2 exp))))
			((eq? (operator exp) '-)  (- (m_value (op1 exp)) (m_value (op2 exp))))
			((eq? (operator exp) '*)  (* (m_value (op1 exp)) (m_value (op2 exp))))
			((eq? (operator exp) '/)  (floor->exact (/ (m_value (op1 exp)) (m_value (op2 exp)))))
			((eq? (operator exp) '%)  (- (m_value (op1 exp)) (* (floor->exact (/ (m_value (op1 exp)) (m_value (op2 exp)))) (op2 exp)) ))
			(else '(error undefined "bad op"))
			)))

(define op1 car)
(define op2 caddr)
(define operator 
	(lambda (e)
		(cadr e)))