(define myappend
  ((lambda (m)
     (m m))
   (lambda (a)
     (lambda (lisa lisb)
       (if (null? lisa)
           lisb
           (cons (car lisa) ((a a) (cdr lisa) lisb)))))))

(define myreverse
  ((lambda (f g)
     (f f g))
   (lambda (r a)
     ;reverse
     (lambda (l)
       (if (null? l)
           l
           ((a a) ((r r) (cdr l)) (cons (car l) '()))))
     )
   (lambda (a)
     ;append
     (lambda (lisa lisb)
       (if (null? lisa)
           lisb
           (cons (car lisa) ((a a) (cdr lisa) lisb))))
     )))

; merge
(define merge
  ((lambda (f)
    (f f))
   (lambda (m)
     (lambda (lisa lisb)
       (cond
         ((null? lisa) lisb)
         ((null? lisb) lisa)
         ((< (car lisa) (car lisb)) (cons (car lisa) ((m m) (cdr lisa) lisb)))
         (else (cons (car lisb) ((m m) (cdr lisb) lisa))))))))

; split
(define split
  ((lambda (f)
     (f f))
   (lambda (s)
     (lambda (lis)
       (cond
         ((null? lis) '(()()))
         ((null? (cdr lis)) (list (list (car lis)) '() ))
         (else (list (cons (car lis) (car ((s s) (cddr lis)))) (cons (cadr lis) (cadr ((s s) (cddr lis))))))
         )))))

; mergesort
(define mymergesort
  ((lambda (f g h)
     (f f g h))
   (lambda (ms m s)
     (lambda (lis)
       (if (or (null? lis) (null? (cdr lis)))
           lis
           ((m m) ((ms ms m s) (car ((s s) lis))) ((ms ms m s) (cadr ((s s) lis))))))
     )
   (lambda (m)
     (lambda (lisa lisb)
       (cond
         ((null? lisa) lisb)
         ((null? lisb) lisa)
         ((< (car lisa) (car lisb)) (cons (car lisa) ((m m) (cdr lisa) lisb)))
         (else (cons (car lisb) ((m m) (cdr lisb) lisa)))))
     )
   (lambda (s)
     (lambda (lis)
       (cond
         ((null? lis) '(()()))
         ((null? (cdr lis)) (list (list (car lis)) '() ))
         (else (list (cons (car lis) (car ((s s) (cddr lis)))) (cons (cadr lis) (cadr ((s s) (cddr lis))))))
         ))
     )
   ))

; replace let with lambdas

(define addstuff
  (lambda (a b c)
    (let ((x ( + a b))
          (y (+ b c)))
      (* x y))))

(define addstuff
  (lambda (a b c)
    ((lambda (x y)
       (* x y))
     (+ a b) (+ b c))))

; multiply a list of numbers
(define fastmult
  (lambda (l)
    (letrec ((loop (lambda (l break return)
                  (cond
                    ((null? l) 1)
                    ((zero? (car l)) (break 0))
                    (else (loop (cdr l) break (lambda (v) (return (* v (car l))))))))))
      (loop l (lambda (v) v) (lambda (v) v)))))

; uhhhhh this doesn't quite work
(define fastmult
  (lambda (l)
    (((lambda (f)
      (f f))
    (lambda (loop)
      (lambda (l break return)
                  (cond
                    ((null? l) 1)
                    ((zero? (car l)) (break 0))
                    (else ((loop loop) (cdr l) break (lambda (v) (return (* v (car l))))))))))
       l (lambda (v) v) (lambda (v) v))))

; replaceall
(define replaceall
  (lambda (x y l)
    (cond
      ((null? l) l)
      ((eq? (car l) x) (cons y (replaceall x y (cdr l))))
      (else (cons (car l) (replaceall x y (cdr l)))))))

(define curry3
  (lambda (f)
    (lambda (x)
      (lambda (y)
        (lambda (z)
          (f x y z))))))

(define uncurry3
  (lambda (f)
    (lambda (x y z)
      (((f x) y ) z))))