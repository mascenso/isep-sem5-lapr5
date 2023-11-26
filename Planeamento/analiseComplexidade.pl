:-dynamic ligacel/2.
:-dynamic ligacelM/3.
:-dynamic m/3.
:-dynamic nlin/1.
:-dynamic melhor_sol_dfs/2.

cria_matriz:-
    retractall(m(_,_,_)),
    retractall(ligacel(_,_)),
    write('Numero de Colunas: '),read(NCol),nl,
    write('Numero de Linhas: '),read(NLin),nl,asserta(nlin(NLin)),
    cria_matriz_0(NCol,NLin),cria_grafo(NCol,NLin),retract(nlin(_)).

cria_matriz_0(1,1):-!,asserta(m(1,1,0)).


cria_matriz_0(NCol,1):-!,asserta(m(NCol,1,0)),NCol1 is NCol-1,nlin(NLin),cria_matriz_0(NCol1,NLin).
cria_matriz_0(NCol,NLin):-asserta(m(NCol,NLin,0)),NLin1 is NLin-1,cria_matriz_0(NCol,NLin1).

/* DFS - Pesquisa em profundidade. Pesquisa a partir do nó inicial e segue o caminho até o fim, depois retrocede. */
dfs(Orig,Dest,Cam,Custo):-
    get_time(Ti),
	dfs2(Orig,Dest,[Orig],Cam),
    length(Cam, Custo),
    get_time(Tf),
    T is Tf-Ti,
    write('Tempo de geracao da solucao:'),write(T),nl.

dfs2(Dest,Dest,LA,Cam):-
    reverse(LA,Cam).

dfs2(Act,Dest,LA,Cam):-
    ligacel(Act,X),\+ member(X,LA),
    dfs2(X,Dest,[X|LA],Cam).

all_dfs(Orig,Dest,LCam):-
    findall(Cam,dfs(Orig,Dest,Cam),LCam).

better_dfs1(Orig,Dest,LCaminho_minlig,Custo):-
    get_time(Ti),
    (better_dfs11(Orig,Dest);true),
    retract(melhor_sol_dfs(LCaminho_minlig,_)),
    length(LCaminho_minlig, Custo),
    get_time(Tf),
    T is Tf-Ti,
    write('Tempo de geracao da solucao:'),write(T),nl.
    
better_dfs11(Orig,Dest):-
    asserta(melhor_sol_dfs(_,10000)),
    dfs(Orig,Dest,LCaminho,Custo1),
    atualiza_melhor_dfs(LCaminho),
    fail.

atualiza_melhor_dfs(LCaminho):-
    melhor_sol_dfs(_,N),
    length(LCaminho,C),
    C<N,retract(melhor_sol_dfs(_,_)),
    asserta(melhor_sol_dfs(LCaminho,C)).
    
shortlist([L],L,N):-!,length(L,N).

shortlist([L|LL],Lm,Nm):-shortlist(LL,Lm1,Nm1),
    length(L,NL),
    ((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).

/* Criar grafo igual as TPs*/
cria_grafo(_,0):-!.

cria_grafo(Col,Lin):-
    cria_grafo_lin(Col,Lin),
    Lin1 is Lin-1,
    cria_grafo(Col,Lin1).

cria_grafo_lin(0,_):-!.

cria_grafo_lin(Col,Lin):-
    m(Col,Lin,0),!,
    ColS is Col+1, 
    ColA is Col-1, 
    LinS is Lin+1,
    LinA is Lin-1,
    ((m(ColS,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColS,Lin)));true)),
    ((m(ColA,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColA,Lin)));true)),
    ((m(Col,LinS,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinS)));true)),
    ((m(Col,LinA,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinA)));true)),
    Col1 is Col-1,
    cria_grafo_lin(Col1,Lin).

cria_grafo_lin(Col,Lin):-Col1 is Col-1,cria_grafo_lin(Col1,Lin).

/* bfs igual as tps*/
bfs(Orig,Dest,Cam,Custo):-
    get_time(Ti),
    bfs2(Dest,[[Orig]],Cam),
    length(Cam, Custo),
    get_time(Tf),
    T is Tf-Ti,
    write('Tempo de geracao da solucao:'),write(T),nl.


bfs2(Dest,[[Dest|T]|_],Cam):-
    reverse([Dest|T],Cam).

bfs2(Dest,[LA|Outros],Cam):-
    LA=[Act|_],
    findall([X|LA],
        (Dest\==Act,ligacel(Act,X),\+ member(X,LA)),
        Novos),
    append(Outros,Novos,Todos),
    bfs2(Dest,Todos,Cam).

all_bfs(Orig,Dest,LCam):-
    findall(Cam,bfs(Orig,Dest,Cam),LCam).

/*Astar*/

aStar(Orig,Dest,Cam,Custo):- 
    get_time(Ti),
	aStar2(Dest,[(_,0,[Orig])],Cam,Custo),
    get_time(Tf),
    T is Tf-Ti,
    write('Tempo de geracao da solucao:'),write(T),nl.

/* quando cheagado ao destino, inverte a lista para apresentar o caminho direito. */
aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo):- 
	reverse([Dest|T],Cam).

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo):- 
	LA=[Act|_],
	findall((CEX,CaX,[X|LA]), 
	(Dest\==Act,ligacelM(Act,X,CustoX),
	\+ member(X,LA),
	CaX is CustoX + Ca,
	estimativa(X,Dest,EstX),
	CEX is CaX +EstX),Novos),
	append(Outros,Novos,Todos),
	/*write('Novos='),write(Novos),nl,*/
	sort(Todos,TodosOrd),
	/*write('TodosOrd='),write(TodosOrd),nl,*/
	aStar2(Dest,TodosOrd,Cam,Custo).

/* calcula o custo para de ir do ponto A ao B.*/
estimativa(cel(X1,Y1),cel(X2,Y2),Estimativa):-
	Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).

/* Cria grafo para o piso indicado tendo em considelaraçao o mapa do piso. */
cria_grafo_piso(Piso):-
    floor_map(Piso, Map),
    cria_grafo_mapa(Map).

cria_grafo_mapa(Map):-
    length(Map, NumLinhas),
    length(Map, NumColunas),
    cria_grafoM(NumLinhas, NumColunas,Map).


/* Cria grafo conforme possiveis caminhos especializado para receber uma matriz dos pisos.
ColS- Coluna Adjacente Seguinte;
ColA- Coluna Adjacente Anterior;
Col- Coluna Atual;
LinS- Linha Adjacente Seguinte;
LinA- Linha Adjacente Anterior;
Lin- Linha Atual;
O Assertz são as conexões das entre as celulas (ligacel), TRUE para continuar sem gerar erro caso nao possa transitar para aquela celula. */
cria_grafoM(_,0,_):-!.
cria_grafoM(Col,Lin,Map):-
	cria_grafo_linM(Col,Lin,Map),
	Lin1 is Lin-1,
	cria_grafoM(Col,Lin1,Map).


cria_grafo_linM(0,_,_):-!.


cria_grafo_linM(Col,Lin, Map):-
	map_value(Col,Lin,Map,V),
    write('v='),write(V),nl,
	not(parede(V)),!,
	ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
	((map_value(ColS,Lin,Map,V),circular(V),(cria_conexoes(Col,Lin,ColS,Lin,1));true)), %M. Horizontal
	((map_value(ColA,Lin,Map,V),circular(V),(cria_conexoes(Col,Lin,ColA,Lin,1));true)), %M. Horizontal
	((map_value(Col,LinS,Map,V),circular(V),(cria_conexoes(Col,Lin,Col,LinS,1));true)), %M. Vertical
	((map_value(Col,LinA,Map,V),circular(V),(cria_conexoes(Col,Lin,Col,LinA,1));true)), %M. Vertical
	Col1 is Col-1,
	cria_grafo_linM(Col1,Lin,Map).


cria_grafo_linM(Col,Lin,Map):-Col1 is Col-1,cria_grafo_linM(Col1,Lin,Map).

map_value(Col,Lin,Map,V):- 
    nth1(Col, Map, Coluna),
    nth1(Lin, Coluna, V).


/*Predicado auxiliar para adicionar conexões com pesos */
cria_conexoes(Col1,Lin1,Col2,Lin2,Peso) :-
    assertz(ligacelM(cel(Col1,Lin1),cel(Col2,Lin2),Peso)),
	assertz(edge(cel(Col1,Lin1),cel(Col2,Lin2))).

/* Predicado para mostrar as conexões criadas -> testar grafo */
mostra_conexoes2 :-
    ligacelM(Cel1, Cel2, Peso),
    write('Conexão de '), write(Cel1), write(' para '), write(Cel2), write(' com peso '), write(Peso), nl,
    fail.
mostra_conexoes.



/*matriz*/
%m(col,lin,valor)
m(1,1,0).
m(2,1,0).
m(3,1,0).
m(4,1,0).
m(5,1,0).
m(6,1,0).
m(7,1,0).
m(8,1,0).
m(1,2,0).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,0).
m(6,2,0).
m(7,2,0).
m(8,2,0).
m(1,3,0).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,0).
m(6,3,0).
m(7,3,0).
m(8,3,0).
m(1,4,0).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,0).
m(6,4,0).
m(7,4,0).
m(8,4,0).
m(1,5,0).
m(2,5,0).
m(3,5,0).
m(4,5,0).
m(5,5,0).
m(6,5,0).
m(7,5,0).
m(8,5,0).
m(1,6,0).
m(2,6,0).
m(3,6,0).
m(4,6,0).
m(5,6,0).
m(6,6,0).
m(7,6,0).
m(8,6,0).
m(1,7,0).
m(2,7,0).
m(3,7,0).
m(4,7,0).
m(5,7,0).
m(6,7,0).
m(7,7,0).
m(8,7,0).

/*Matriz astar*/

floor_map(5, [
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0]
]).

floor_map(4, [
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0]
]).
floor_map(3, [
    [0, 0, 0],
    [0, 0, 0],
    [0, 0, 0]
]).
floor_map(6, [
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0]
]).
floor_map(7, [
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0]
]).
floor_map(8, [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0]
]).

floor_map(9, [
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0]
]).

floor_map(10, [
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]).

floor_map(11, [
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]).

%valores que para representar portas, corredores, elevadores, etc
circular(N):- member(N,[0]).

%valores da matriz para representar paredes
parede(N):- member(N,[1]).

%valores que para representar portas
porta(N):-member(N,[2]).

%valores que para representar elevadores
elev(N):-member(N,[3]).

%valores que para representar passagens
passagem(N):-member(N,[4]).