/* ## ALGORITMOS PARA O CALCULO DOS CAMINHOS/TRAJECTOS DO ROBDRONEGO NUM PISO. GRANULARIDADE PEQUENA ## */

:-dynamic ligacel/3.
:-dynamic edge/2.

:- consult('BC_trajectos.pl').

/* Cria grafo conforme possiveis caminhos (0, 4, 5, 1.4, 1.3, 0.2, 0.3, 0.4, 0.5). Ver circular(N) em BC_trajectos.
ColS- Coluna Adjacente Seguinte;
ColA- Coluna Adjacente Anterior;
Col- Coluna Atual;
LinS- Linha Adjacente Seguinte;
LinA- Linha Adjacente Anterior;
Lin- Linha Atual;
O Assertz são as conexões das entre as celulas (ligacel), TRUE para continuar sem gerar erro caso nao possa transitar para aquela celula. */
cria_grafo(_,0):-!.
cria_grafo(Col,Lin):-cria_grafo_lin(Col,Lin),Lin1 is Lin-1,cria_grafo(Col,Lin1).

cria_grafo_lin(0,_):-!.
cria_grafo_lin(Col,Lin):-
	m(Col,Lin,V),
	not(parede(V)),!,
	ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
	((m(ColS,Lin,V),circular(V),(cria_conexoes(Col,Lin,ColS,Lin,1));true)), %M. Horizontal
	((m(ColA,Lin,V),circular(V),(cria_conexoes(Col,Lin,ColA,Lin,1));true)), %M. Horizontal
	((m(Col,LinS,V),circular(V),(cria_conexoes(Col,Lin,Col,LinS,1));true)), %M. Vertical
	((m(Col,LinA,V),circular(V),(cria_conexoes(Col,Lin,Col,LinA,1));true)), %M. Vertical
	((m(ColS,LinS,V),circular(V),(cria_conexoes(Col,Lin,ColS,LinS,sqrt(2)));true)), %M. Diagonal
	((m(ColA,LinS,V),circular(V),(cria_conexoes(Col,Lin,ColA,LinS,sqrt(2)));true)), %M. Diagonal
	((m(ColS,LinA,V),circular(V),(cria_conexoes(Col,Lin,ColS,LinA,sqrt(2)));true)), %M. Diagonal
	((m(ColA,LinA,V),circular(V),(cria_conexoes(Col,Lin,ColA,LinA,sqrt(2)));true)), %M. Diagonal
	Col1 is Col-1,
	cria_grafo_lin(Col1,Lin).

cria_grafo_lin(Col,Lin):-Col1 is Col-1,cria_grafo_lin(Col1,Lin).

/*Predicado auxiliar para adicionar conexões com pesos */
cria_conexoes(Col1,Lin1,Col2,Lin2,Peso) :-
    assertz(ligacel(cel(Col1,Lin1),cel(Col2,Lin2),Peso)),
	assertz(edge(cel(Col1,Lin1),cel(Col2,Lin2))).

/* Predicado para mostrar as conexões criadas -> testar grafo */
mostra_conexoes :-
    ligacel(Cel1, Cel2, Peso),
    write('Conexão de '), write(Cel1), write(' para '), write(Cel2), write(' com peso '), write(Peso), nl,
    fail.
mostra_conexoes.

/* DFS - Pesquisa em profundidade. Pesquisa a partir do nó inicial e segue o caminho até o fim, depois retrocede. */
dfs(Orig,Dest,Cam):-
	dfs2(Orig,Dest,[Orig],Cam).

dfs2(Dest,Dest,LA,Cam):-reverse(LA,Cam).

dfs2(Act,Dest,LA,Cam):-
	edge(Act,X),
    \+ member(X,LA),
	dfs2(X,Dest,[X|LA],Cam).


/* Algoritmo para encontrar todos os caminhos entre dois pontos.
Devolve uma lista com todos os caminhos possiveis */
all_dfs(Orig,Dest,LCam):-findall(Cam,dfs(Orig,Dest,Cam),LCam).


/* Algoritmo para encontrar o melhor o caminho entre dois pontos. */
better_dfs(Orig,Dest,Cam):-all_dfs(Orig,Dest,LCam), shortlist(LCam,Cam,_).

shortlist([L],L,N):-!,length(L,N).
shortlist([L|LL],Lm,Nm):-
	shortlist(LL,Lm1,Nm1),
	length(L,NL),
	((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).


/* BFS - Pesquisa em largura. Primeiro pesquisa os nós adajacentes ao nó inicial antes de ir ver os nós vizinhos do nó vizinho.
A decisão sobre qual ramo seguir ser feita com base num critério de decisão local. Em caso de indecisão
vamos pelo caminho mais promissor, tendo em conta o custo/ganho do caminho.*/
bfs(Orig, Dest, Cam):- bfs2(Dest, [[Orig]], Cam).

bfs2(Dest, [[Dest|T]|_], Cam):-reverse([Dest|T], Cam).

bfs2(Dest, [LA|Outros], Cam):-
    LA = [Act|_],
    findall([X|LA], (Dest \== Act, edge(Act, X), \+ member(X, LA)),Novos),
    append(Outros, Novos, Todos),
    bfs2(Dest, Todos, Cam).


/* BFS - Best First Search sem custo */

bestfs(Orig,Dest,Cam):- bestfs2(Dest,[Orig],Cam).

%condicao final: destino = nó à cabeça do caminho actual
bestfs2(Dest,[Dest|T],Cam):-!,
    %caminho está invertido
    reverse([Dest|T],Cam). 

bestfs2(Dest,LA,Cam):- LA=[Act|_], % substituir por member(Act,LA), caso haja cortes nos caminhos
    %calcular todos os nós adjacentes não visitados e guarda um tuplo com a estimativa e novo caminho
    findall((EstX,[X|LA]), ((edge(Act,X);edge(X,Act)), \+ member(X,LA), estimativa(X,Dest,EstX)), Novos),
    %ordena pela estimativa
    sort(Novos,NovosOrd),
    %extrai o melhor (header)
    NovosOrd = [(_,Melhor)|_],
    %chama-se recursivamente
    bestfs2(Dest,Melhor,Cam).


/* BFS - Best First Search com Custo */
/*
bestfs(Orig,Dest,Cam,Custo):- bestfs2(Dest,[Orig],Cam,Custo).

bestfs2(Dest,(Custo,[Dest|T]),Cam,Custo):-!,
    reverse([Dest|T],Cam). 

bestfs2(Dest,(Ca,LA),Cam,Custo):- member(Act,LA),
    findall((EstX,CaX,[X|LA]), ((ligacel(Act,CX);ligacel(X,Act,CX)), \+ member(X,LA), estimativa(X,Dest,EstX)),CaX is Ca+CX, Novos),
    sort(Novos,NovosOrd),
    NovosOrd = [(_,CM,Melhor)|_],
    bestfs2(Dest,(CM,Melhor),Cam,Custo).
*/

/* Algoritmo para encontrar o caminho mais curto entre dois pontos
Orig - Origem
Dest - Destino
Cam - Caminho
Custo - Custo do trajecto.
Devolve uma lista com o caminho percorrido e o repetivo custo associado.*/
aStar(Orig,Dest,Cam,Custo):- 
	aStar2(Dest,[(_,0,[Orig])],Cam,Custo).

%quando cheagado ao destino, inverte a lista para apresentar o caminho direito.
aStar2(Dest,[(_,Custo,[Dest|T])|_],Cam,Custo):- 
	reverse([Dest|T],Cam).

aStar2(Dest,[(_,Ca,LA)|Outros],Cam,Custo):- 
	LA=[Act|_],
	findall((CEX,CaX,[X|LA]), 
	(Dest\==Act,ligacel(Act,X,CustoX),
	\+ member(X,LA),
	CaX is CustoX + Ca,
	estimativa(X,Dest,EstX),
	CEX is CaX +EstX),Novos),
	append(Outros,Novos,Todos),
	write('Novos='),write(Novos),nl,
	sort(Todos,TodosOrd),
	write('TodosOrd='),write(TodosOrd),nl,
	aStar2(Dest,TodosOrd,Cam,Custo).

/* calcula o custo para de ir do ponto A ao B.*/
estimativa(cel(X1,Y1),cel(X2,Y2),Estimativa):-
	Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).