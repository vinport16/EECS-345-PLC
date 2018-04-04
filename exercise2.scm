; Vincent Portelli VLP25

; 1. insert takes a number and a list of numbers in order and inserts the number in the proper place. 
(define insert-cps
	(lambda (x lis exit)
		(cond
			((null? lis) (exit (list x)) )
			((> (car lis) x) (exit (cons x lis)))
			(else (insert-cps x (cdr lis) (lambda (x) (exit (cons (car lis) x) ) ) ) )
			)))

(define insert
	(lambda (x lis)
		(insert-cps x lis (lambda (x) x))))

; 2. merge takes two lists of numbers that are in order and returns a list that contains the combination of both lists in order.
(define merge-cps
	(lambda (lisa lisb exit)
		(cond
			((null? lisa) (exit lisb))
			((null? lisb) (exit lisa))
			((< (car lisa) (car lisb)) (merge-cps (cdr lisa) lisb (lambda (x) (exit (cons (car lisa) x)))) )
			(else                      (merge-cps lisa (cdr lisb) (lambda (x) (exit (cons (car lisb) x)))) )
			)))

(define merge
	(lambda (lisa lisb)
		(merge-cps lisa lisb (lambda (x) x))))

; 3. removedups takes a list of atoms and removes any atom that is a repeat of the atom that immediately precedes it.
(define removedups-cps
	(lambda (lis exit)
		(cond
			((null? lis) (exit lis) )
			((null? (cdr lis)) (exit lis))
			((eq? (car lis) (cadr lis)) (removedups-cps (cdr lis) (lambda (x) (exit x)) ))
			(else                       (removedups-cps (cdr lis) (lambda (x) (exit (cons (car lis) x ))) ))
			)))

(define removedups
	(lambda (lis)
		(removedups-cps lis (lambda (v) v))))

; 4. numparens takes a list and returns the number of pairs of parentheses
(define numparens-cps
	(lambda (lis exit)
		(cond
			((null? lis) (exit 1))
			((list? (car lis)) (numparens-cps (cdr lis) (lambda (v) (+ v (numparens-cps (car lis) exit)))))
			(else (numparens-cps (cdr lis) exit))
			)))

(define numparens
	(lambda (lis)
		(numparens-cps lis (lambda (v) v))))

; 5. dup* takes a list and duplicates all contents, including any sublists
(define dup*-cps
	(lambda (lis exit)
		(cond
			((null? lis) (exit lis))
			((list? (car lis)) (dup*-cps (cdr lis) (lambda (v) (exit (cons (dup*-cps (car lis) (lambda (z) z) ) (cons (dup*-cps (car lis) (lambda (y) y) ) v) ))) ))
			(else              (dup*-cps (cdr lis) (lambda (v) (exit (cons (car lis) (cons (car lis) v))))))
			)))

(define dup*
	(lambda (lis)
		(dup*-cps lis (lambda (v) v))))

(dup* '(a b (c d (e) ) (f g) h) )

; 6. removedups* takes a list, that can contain sublists, and removes any atom that is the repeat of the atom that immediately precedes it in the same sublist.
(define removedups*-cps
	(lambda (lis exit)
		(cond
			((null? lis) (exit lis))
			((list? (car lis)) (removedups*-cps (cdr lis) (lambda (v) (exit (cons (removedups*-cps (car lis) (lambda (z) z)) v)))))
			((null? (cdr lis)) (exit lis))
			((eq? (car lis) (cadr lis)) (removedups*-cps (cdr lis) exit))
			(else (removedups*-cps (cdr lis) (lambda (z) (exit (cons (car lis) z)))))
			)))

(define removedups*
	(lambda (lis)
		(removedups*-cps lis (lambda (r) r))))

(removedups* '(a a a a b (b b b b(c c c)) (d d d) e e f g h))

; 7. mergesort takes a list of numbers and returns a sorted version. If you recall the merge sort algorithm, you use the CPS version of split from lecture to divide the input list into two lists, you recursively call mergesort on each sublist, and then you call merge on the two lists returned by the recursive calls to mergesort.

(define split-cps
	(lambda (lis return)
		(cond
			((null? lis) (return lis lis))
			((null? (cdr lis)) (return lis (cdr lis)))
			(else (split-cps (cddr lis) (lambda (v1 v2) (return (cons (car lis) v1) (cons (cadr lis) v2)))))
			)))

(define mergesort-cps
	(lambda (lis exit)
		(cond
			((or (null? lis) (null? (cdr lis))) (exit lis))
			(else (split-cps lis (lambda (x y)
				(mergesort-cps x (lambda (z)
					(mergesort-cps y (lambda (k)
						(merge-cps k z exit)) )) )) ) )
			)))

(define mergesort
	(lambda (lis)
		(mergesort-cps lis (lambda (q) q))))

(mergesort '( 26 37 84 64 39 55 29 ) )

; 8. replaceatoms takes two lists. The first list can contain sublists, but the second list is a single list of atoms. The output should be the first list, but each atom of the first list, from left to right, is replaced by the corresponding atom of the second list, until the second list runs out of atoms.
(define replaceatoms-cps
	(lambda (lisa lisb exit)
		(cond
			((null? lisa) (exit lisa lisb))
			((null? lisb) (exit lisa lisb))
			((list? (car lisa)) (replaceatoms-cps (car lisa) lisb (lambda (x y)
				(replaceatoms-cps (cdr lisa) y (lambda (a b)
					(exit (cons x a) b)))) ))
			(else (replaceatoms-cps (cdr lisa) (cdr lisb) (lambda (s d)
				(exit (cons (car lisb) s) d)) ))
			)))
(define replaceatoms
	(lambda (lisa lisb)
		(replaceatoms-cps lisa lisb (lambda (t x) t))))

(replaceatoms '(a b c d e f g h i) '(x y z p d q x))
(replaceatoms '(a (b) c) '(x y z r))
(replaceatoms '(a b c (d e (f)) g h i) '(x y z p d q x))

; now use call/cc
; 9. suffix takes an atom and a list and returns a list containing all elements that occur after the last occurrence of the atom.
(define call/cc call-with-current-continuation)

(define suffix-cc
	(lambda (a lis break)
		(cond
			((null? lis) lis)
			((eq? a (car lis)) (break (suffix-cc a (cdr lis) break)))
			(else (cons (car lis) (suffix-cc a (cdr lis) break)))
			)))

(define suffix
	(lambda (a lis)
		(call/cc
			(lambda (break)
				(suffix-cc a lis break)))))

(suffix 'a '(w w a b c d a lol haha a ur cool) )

; 10. emptysublists takes an atom and a list containing sublists. The output list should be the same as the input list except that any sublist (including the main list) that contains the given atom should be emptied of all non-list atoms.
(define call/cc call-with-current-continuation)

(define emptysublists-cc
	(lambda (a lis break)
		(cond
			((null? lis) lis)
			((list? (car lis)) (cons (emptysublists a (car lis)) (emptysublists-cc a (cdr lis) break)))
			((eq? a (car lis)) (break '() ))
			(else (cons (car lis) (emptysublists-cc a (cdr lis) break)) )
			)))

(define emptysublists
	(lambda (a lis)
		(call/cc
			(lambda (break)
				(emptysublists-cc a lis break)))))
















