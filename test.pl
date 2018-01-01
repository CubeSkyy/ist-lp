% par(N):- N mod 2 =:= 0 .
%
% filtra_pares([],[]) :- !.
%
% filtra_pares([H1|R1],[H1|R2]) :-
%   par(H1), !,
%   filtra_pares(R1,R2).
%
% filtra_pares([H1|R1],R2) :-
%   \+ par(H1), !,
%   filtra_pares(R1,R2).

% substitui(_,_,[],[]):- !.
%
% substitui(X,Y,[H1|R1],[H2|R2]) :-
%   H1 =:= H2,
%   substitui(X,Y,R1,R2),!.
%
% substitui(X,Y,[H1|R1],[H2|R2]) :-
%   \+ H1 =:= H2,
%   X =:= H1,
%   Y =:= H2,
%   substitui(X,Y,R1,R2).

penultimo([X,_|[]], X).
penultimo([_,Y|Ys], X) :- penultimo([Y|Ys], X).
