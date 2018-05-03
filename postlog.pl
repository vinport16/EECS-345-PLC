% myappend

myappend([],L,L).
myappend([H|T],L,[H|S]) :- myappend(T,L,S).

% contains
%  (define contains
%    (lambda (x lis)
%      (cond
%        ((null? lis) F)
%        ((eq? x (car lis)) T)
%        (else (contains x (cdr lis))))))

contians(A, [A|_]).
contians(A, [_|T]) :- contains(A,T).

% add X before A in the list
insertbefore(_,_,[],[]).
insertbefore(A,X,[A|T],[X,A|T]).
insertbefore(A,X,[H|T],[H|S]) :- insertbefore(A,X,T,S).


% insertbeforeall: use the cut: !
% the cut is always true. you cant backtrack past the cut
% once a cut is reached, it wont try more rules and it wont change values in front of it

insertbeforeall(_,[],[]) :- !.
insertbeforeall(A,[B|T],[A,B|S]) :- insertbeforeall(A,T,S).


% myreverse
myreverse([],[]).
myreverse([A|B],X) :- myappend(Z,[A],X), myreverse(B,Z).

% flatten: flatten(a,[[b],[[c,d],e]], [a,b,c,d,e]).
flatten([],[]).
flatten([[A]|T],Q) :- flatten([A],B), flatten(T,G), myappend(B,G,Q).
flatten([H,T],[H,P]) :- flatten(T,P).

% factorial: us 'is' for math
%factorial(0,1).
%factorial(N,X) :- factorial (M,Y), N is M+1, X is N*Y.