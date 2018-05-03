#lang racket

(define-syntax debug
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(begin (print ,(cadr slist) ) ,(cadr slist)))))

(define-syntax foreachdo
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(map (lambda (,(cadr slist)) ,(caddr slist)) ,(cadddr slist)))))

(define-syntax just
  (lambda (syn)
    (define slist (syntax->list syn))
    (datum->syntax syn `(list 'just ,(cadr slist)))))

(define-syntax one
  (lambda (syn)
    1))

(foreachdo x (* x x) '(1 2 3 4) )