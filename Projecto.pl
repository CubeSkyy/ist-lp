% Miguel Coelho, 87687, LEIC-T, 05/12/2017

:- include('SUDOKU').


tira_num_aux(Num,Puz,Pos,N_Puz) :-
  puzzle_ref(Puz,Pos,[Num]),         % Se o conteudo de Pos e Num, entao vamos mudar Pos para a lista vazia.
  puzzle_muda(Puz,Pos,[],N_Puz),!.

tira_num_aux(Num,Puz,Pos,Puz) :-
  puzzle_ref(Puz,Pos,Cont),
  \+ member(Num,Cont),!.           % Se Num nao estiver no conteudo de Pos, entao nao existe mudanca(N_Puz = Puz)

tira_num_aux(Num,Puz,Pos,N_Puz) :-
  puzzle_ref(Puz,Pos,Cont),                    % Se Num estiver no conteudo de Pos e o conteudo nao e apenas Num,
  subtract(Cont,[Num],Result),                 %  entao retiramos Num ao conteudo,
  puzzle_muda_propaga(Puz,Pos,Result,N_Puz),!. %  e propagamos a mudanca em Puz sendo N_Puz o puzzle propagado.


tira_num(Num,Puz,Posicoes,N_Puz) :-
  percorre_muda_Puz(Puz,tira_num_aux(Num),Posicoes,N_Puz). % Aplica o predicado a lista de posicoes.


puzzle_muda_propaga(Puz,Pos,[H|T],N_Puz) :- T \== [], puzzle_muda(Puz,Pos,[H|T],N_Puz), !.
% Se a lista nao for unitaria, muda o conteudo de Pos para a lista [H|T]


puzzle_muda_propaga(Puz,Pos,[H],N_Puz) :- % Se a lista a mudar for unitaria,
  puzzle_muda(Puz,Pos,[H],Puz_2),         %  efectuamos a mudanca,
  posicoes_relacionadas(Pos,Posicoes),    %  encontramos a posicoes relacionadas com Pos,
  tira_num(H,Puz_2,Posicoes,N_Puz).       %  e retiramos todas as ocorrencias de num nessas posicoes.


% removel_nao_uni, tem como argumentos duas listas, a primeira e uma lista de listas e a segunda e o resultado de retirar
% a primeira lista todas as listas nao unitarias desta.

removel_nao_uni([],[]).               % Base

removel_nao_uni([[]|R1],R2) :-        % Se a cabeca for uma lista sem elementos, a resposta e o predicado aplicado ao resto da lista.
   removel_nao_uni(R1,R2).

removel_nao_uni([[H]|R1],[[H]|R2]) :- % Se a cabeca for uma lista unitaria, a resposta e a cabeca e o predicado aplicado ao resto da lista.
  removel_nao_uni(R1,R2).

removel_nao_uni([[_,_|_]|R1],R2) :-   % Se a cabeca for uma lista nao unitaria e nao vazia o resultado e o predicado aplicado ao resto da lista.
  removel_nao_uni(R1,R2).


possibilidades(Pos,Puz,Poss) :-
  posicoes_relacionadas(Pos,Posicoes),
  conteudos_posicoes(Puz,Posicoes,Cont_aux), % Cont_aux e o conteudo das posicoes relacionadas com Pos.
  removel_nao_uni(Cont_aux,Cont),            % Removemos as listas nao unitarias a Cont_aux, sendo Cont a lista resultante.
  append(Cont,Res),                          % Concatena a lista de listas numa so lista, sendo Res a lista resultante.
  numeros(L),
  subtract(L,Res,Poss).                      % Remove os valores de Res a lista criada por numeros(L), sendo Poss a lista resultante.


inicializa_aux(Puz,Pos,Puz) :-    % Se o conteudo de Pos em Puz for uma lista nao vazia, entao nao existe mundaca(Puz=N_Puz)
  puzzle_ref(Puz,Pos,[_|_]),!.

inicializa_aux(Puz,Pos,N_Puz) :-
  puzzle_ref(Puz,Pos,[]),                   % Se o conteudo de Pos em Puz for uma lista unitaria,
  possibilidades(Pos,Puz,Poss),             %  procuramos as possibilidades para essa posisao,
  puzzle_muda_propaga(Puz,Pos,Poss,N_Puz).  %  e Efectuamos a mudanca e propagacoes.


inicializa(Puz,N_Puz) :-
  todas_posicoes(Todas_Posicoes),
  percorre_muda_Puz(Puz,inicializa_aux,Todas_Posicoes,N_Puz). % Efectuamos inicializa_aux ao longo de todas as posicoes do puzzle


% procura_pos(Puz,Num,Posicoes,Pos_Num), onde Puz e um puzzle, Num e um numero, Posicoes e uma lista de posicoes, e
%   Pos_Num e a primeira posicao da lista de posicoes onde o conteudo do puzzle e a lista unitaria com o elemento Num.
% Sera falso se Num nao existir nos conteudos de Posicoes em Puz.

procura_pos(Puz,Num,[P1|_],P1) :-
  puzzle_ref(Puz,P1,Cont),
  member(Num,Cont),!.                   % Se Num pertencer ao conteudo de P1, Cont, entao Pos_Num e P1

procura_pos(Puz,Num,[_|R1],Pos_Num) :-  % Caso contrario, chamamos o predicado para o resto da lista de posicoes.
  procura_pos(Puz,Num,R1,Pos_Num).


so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num) :-
  conteudos_posicoes(Puz,Posicoes,Cont_pos), % Cont_pos e uma lista de listas com os valores de Posicoes em Puz.
  append(Cont_pos,Cont_conc),                % Cont_conc sera a lista com os valores de Cont_pos concatenados.
  select(Num,Cont_conc,Res),                 % Res e a lista Cont_conc depois de removida a primeira ocorrencia de Num.
  \+ member(Num,Res),                        % Se Num nao pertencer a Res, entao so havia uma occurencia de Num em Cont_conc
  procura_pos(Puz,Num,Posicoes,Pos_Num).     % Visto que so existe uma Posicao com Num, ao usarmos o predicado procura_pos, Pos_Num sera a unica posicao com Num.


inspecciona_num(Posicoes,Puz,Num,Puz) :-
 \+ so_aparece_uma_vez(Puz,Num,Posicoes,_), !.  % Se Num aparece mais doque uma vez em Posicoes, entao nao existe mudanca

inspecciona_num(Posicoes,Puz,Num,Puz) :-
  so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num), % Se Num aparece apenas uma vez em Posicoes,
  puzzle_ref(Puz,Pos_Num,[_]), !.               %  mas o conteudo da posicao onde aparece e uma lista unitaria, nao existe mudanca.

inspecciona_num(Posicoes,Puz,Num,N_Puz) :-
  so_aparece_uma_vez(Puz,Num,Posicoes,Pos_Num), % Caso contrario,
  puzzle_muda_propaga(Puz,Pos_Num,[Num],N_Puz). %  mudamos o conteudo de Pos_Num para [Num] e propagamos as mudancas.


inspecciona_grupo(Puz,Gr,N_Puz) :-
  numeros(L),
  percorre_muda_Puz(Puz,inspecciona_num(Gr),L,N_Puz). %Aplica inspecciona_num a lista de Posicoes (Gr).


inspecciona(Puz,N_Puz) :-
  grupos(Gr),
  percorre_muda_Puz(Puz,inspecciona_grupo,Gr,N_Puz).


% n_duplicado, e falso se existirem elementos repetidos em L,
% se nao existirem, entao L_sort e a lista L ordenada.

n_duplicado(L,L_sort) :-
  sort(L, L_sort),          % L_sort e a lista L ordenada com duplicados removidos.
  length(L, A),             % Se o tamanho de L
  length(L_sort, A).        % For igual ao tamanho de L_sort, entao nao existiam duplicados e L_sort sera L ordenada.


grupo_correcto(Puz,Nums,Gr) :-
  conteudos_posicoes(Puz,Gr,Cont1),
  append(Cont1,Cont2),   % lista dos conteudos das pos. de Gr concatenada.
  n_duplicado(Cont2,Cont2_sort), % verifica se nao existem duplicados e se nao, Cont2_sort sera a lista ordenada
  msort(Nums,Nums_sort),  % Nums_sort e a lista ordenada, sem remover duplicados.
  Nums_sort == Cont2_sort. % Se essa lista for igual a Count2 ordenada, entao todos os numeros de Num aparecem uma unica vez em Cont2.


solucao_aux(_,_,[]) :- !.          % Condicao de paragem

solucao_aux(Puz,Nums,[H1|R1]) :-
    grupo_correcto(Puz,Nums,H1),   % Verifica se o grupo e correcto
    solucao_aux(Puz,Nums,R1).      % Chama o predicado ao resto dos grupos.
                                   % Para ser verdadeiro, todos os grupos tem que passar no predicado grupo_correcto.

solucao(Puz) :-
  numeros(Nums),
  grupos(Gr),
  solucao_aux(Puz,Nums,Gr).       % Chama o predicado auxiliar para todos os grupos e para todos os numeros possiveis para Puz.


resolve_aux(Puz,Puz_init) :-           % Efectua a inicializacao e a inspecao de um Puz.
  inicializa(Puz,Puz_aux),
  inspecciona(Puz_aux,Puz_init).


% nao_unitaria, tem como argumentos Puz, Pos, Cont, onde Puz e um puzzle, Pos e Cont seram uma posicao e o seu conteudo, respectivamente, apenas
% se Cont for uma lista com pelo menos dois elementos. Chamando o predicado com Puz, este percorre todas estas posicoes em Puz.

nao_unitaria(Puz,X,[H1,H2|T1]):-
  todas_posicoes(Todas_Posicoes),
  member(X,Todas_Posicoes),          % Percorre todas as posicoes de Puz
  puzzle_ref(Puz,X,[H1,H2|T1]).      % E unifica apenas com as posicoes que tenham como conteudo uma lista com pelo menos dois elementos.


resolve(Puz,Sol) :-           % Em alguns casos o Puzzle pode ser resolvido apenas com a inicializacao e inspecao.
  resolve_aux(Puz,Sol),
  solucao(Sol),!.

resolve(Puz,Sol) :-                             % Caso nao seja,
  resolve_aux(Puz,Puz_aux),                     %  inicializamos e inspecionamos o puzzle.
  nao_unitaria(Puz_aux,Pos,Cont),               % nao_unitaria percorre as posicoes de Puz que tem conteudo uma lista com pelo menos dois elementos
  member(X,Cont),                               %  e member percore os elementos dessa posicao um a um.
  puzzle_muda_propaga(Puz_aux,Pos,[X],N_Puz),   % Tentamos propagar a esse elemento,
  inspecciona(N_Puz,N_Puz_aux),                 %  e inspecionamos o puzzle resultante, depois
  solucao(N_Puz_aux),!,                         %  verificamos se e uma solucao. Caso nao seja, tentamos propagar o proximo elemento.
  Sol = N_Puz_aux.                              % Se nao obtivermos uma solucao repetimos para a proxima posicao com conteudo nao unitario
                                                %  e se obtivermos uma solucao atribuimos esse puzzle a Sol.
