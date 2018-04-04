; recursion
(define fact0rial
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact0rial (- n 1)))
        )))

; try to make factorial without calling factorial
(define crash
  (lambda (n)
    (error "crashed!")))

;(define error (lambda (v) v))

;(call-with-current-continuation
; (lambda (break)
;   (set! error break)))

(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n ((lambda (n)
                (if (zero? n)
                    1
                    (* n ((lambda (n)
                            (if (zero? n)
                                1
                                (* n (crash (- n 1)))
                                ))
                          (- n 1)))
                    ))
              (- n 1)))
        )))


(define factorial2
  ( (lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (- n 1)))))
      ) ((lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (- n 1)))))
      ) ((lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (- n 1)))))
      ) ((lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (- n 1)))))
      ) ((lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (- n 1)))))
      ) crash))))))

(define factorial3
  ((lambda (mkfact)
    (mkfact (mkfact (mkfact (mkfact (mkfact (mkfact (mkfact crash)))))))) ;number of mkfact determines number of layers of factorial possible
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (- n 1))))))))


(define factorial4
  ((lambda (mkfact)
    (mkfact mkfact))
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n ((fact fact) (- n 1))))))))

(define myappend
  ((lambda (mkappend)
     (mkappend mkappend))
   (lambda (pend)
     (lambda (lisa lisb)
       (if (null? lisa)
           lisb
           (cons (car lisa) ((pend pend) (cdr lisa) lisb)))))))

(define removeall*
  ((lambda (mkrem)
     (mkrem mkrem))
   (lambda (rem)
     (lambda (x lis)
       (cond
         ((null? lis) lis)
         ((list? (car lis)) (cons ((rem rem) x (car lis)) (f f) x (cdr l))) 
         ((eq? (car lis) x) ((rem rem) x (cdr lis)))
         (else (cons (car lis) ((rem rem) x (cdr lis)))) ) ))))

(define myreverse
  ((lambda (m)
     (m m))
   (lambda (f)
     (lambda (lis)
       (if (null? lis)
           '()
           ( ((((lambda (mkappend)
                 (mkappend mkappend))
               (lambda (pend)
                 (lambda (lisa lisb)
                   (if (null? lisa)
                       lisb
                       (cons (car lisa) ((pend pend) (cdr lisa) lisb))))))) ((f f) (cdr lis)) (cons (car lis) '()))))))))
           