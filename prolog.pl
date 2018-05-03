% facts must be grouped together
% facts must be lowercase, ending with a period

% run with `swipl` then `['prolog.pl'].`

male(ron).
male(percy).
male(bill).
male(charlie).
male(fred).
male(george).
male(arthur).
male(harry).
male(albusjr).
male(jamesjr).
male(hugo).

female(ginny).
female(molly).
female(fleur).
female(lilyjr).
female(lily).
female(petunia).
female(hermione).

parentof(harry,albusjr).
parentof(harry,jamesjr).
parentof(ginny,albusjr).
parentof(ginny,jamesjr).

parentof(ron,hugo).
parentof(hermione,hugo).

parentof(arthur,ron).
parentof(molly,ron).
parentof(arthur,george).
parentof(molly,george).
parentof(arthur,fred).
parentof(molly,fred).
parentof(arthur,ginny).
parentof(molly,ginny).
parentof(arthur,percy).
parentof(molly,percy).
parentof(arthur,bill).
parentof(molly,bill).
parentof(arthur,charlie).
parentof(molly,charlie).

married(harry,ginny).
married(ron,hermione).
married(fleur,bill).
married(molly,arthur).

% rules are of the form A:- B,C,D.
% this means `B and C and D imply A` = `B^C^D -> A` = `¬B v ¬C v ¬D -> ¬A`

grandparentof(X,Y) :- parentof(Z,Y),parentof(X,Z).

% now you can ask, `grandparentof(X,albusjr).` -> `X = arthur; X = molly.`

sibling(X,Y) :- parentof(Z,X),parentof(Z,Y),X\=Y.

cousin(X,Y) :- parentof(A,X),parentof(B,Y),sibling(A,B),A\=B.

auntof(X,Y) :- parentof(A,Y),sibling(X,A),female(X).
auntof(X,Y) :- parentof(A,Y),married(A,B),sibling(B,X),female(X). % something wrong with this (?)
auntof(X,Y) :- parentof(A,Y),married(B,A),sibling(B,X),female(X).

uncleof(X,Y) :- parentof(A,Y),sibling(X,A),male(X).

% lists in prolog are [car | cdr]
% myappend(list1, list2, result).

myappend([],L,L).
myappend([H|T],L,[H|S]) :- myappend(T,L,S).





















