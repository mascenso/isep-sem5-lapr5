/* ## ALGORITMOS PARA O CALCULO DOS CAMINHOS/TRAJECTOS DO ROBDRONEGO ENTRE EDIFICIOS E PISOS. ## */

:-dynamic ligacel/3.
:-dynamic edge/2.

/*:- consult('BC_trajectos.pl').
:- consult('BC_RobDroneGo.pl'). */

/* ################################################################### */
/* ########## ALGORITMOS AO NIVEL DO CAMPUS (GRANDE ESCALA) ########## */
/* ################################################################### */

/* ----------------- ALGORTIMOS AO NIVEL DO EDIFICIO ----------------- */

/* Algoritmo para encontrar um caminho entre dois edificios, EdOr e EdDest.
Devolve uma lista de edificios percorridos
EdOr -Edificio Origem;
EdDest - Edifcio Destino; 
Lcam - Lista de edificios percorridos; */
caminho_edificios(EdOr,EdDest,LCam):- caminho_edificios2(EdOr,EdDest,[EdOr],LCam).

/* Condição de paragem. Se o Edx=EdDest é sinal que chegou ao destino. 
É necessário inverter a lista para aparecer pela ordem de passagem. */
caminho_edificios2(EdX,EdX,LEdInv,LCam):-!, reverse(LEdInv,LCam).

caminho_edificios2(EdAct,EdDest,LEdPassou,LCam):- 
    /* verfica se há ligação entre os edificios e um edificio intermédio. */
    (liga(EdAct,EdInt);liga(EdInt,EdAct)), 
    /* verfica se já passou pelo edificio intermédio */
    \+member(EdInt,LEdPassou),
    /* recursividade para esgotar as opções. */
    caminho_edificios2(EdInt,EdDest,[EdInt|LEdPassou],LCam).


/* Algoritmo para encontrar todos os caminhos entre dois edificios,EdOr e EdDest.
Devolve uma lista com todos os caminhos possiveis*/
todos_caminhos_edificios(EdOr,EdDest,LTCamEd):-
    findall(LCam,caminho_edificios(EdOr,EdDest,LCam),LTCamEd).


/* ------------------- ALGORTIMOS AO NIVEL DO PISO ------------------- */

/* Algoritmo para encontrar todos os caminhos entre dois pisos,PisoOr e PisoDest.
Devolve uma lista de edificios percorridos e uma lista das ligaçoes (elevadores e|ou corredores)
PisoOr - Piso de origem;
PisoDest - Piso de destino;
LCam - Lista de caminhos percorrido;
LLig - Lista de ligaçoes percorridas;
*/
caminho_pisos(PisoOr,PisoDest,LCam,LLig):- 
    pisos(EdOr,LPisosOr),
    member(PisoOr,LPisosOr),
    pisos(EdDest,LPisosDest),
    member(PisoDest,LPisosDest), 
    caminho_edificios(EdOr,EdDest,LCam),
    segue_pisos(PisoOr,PisoDest,LCam,LLig).

/* condiçao de paragem caso tenha cheagado ao destino. */
segue_pisos(PisoDest,PisoDest,_,[]).

segue_pisos(PisoDest1,PisoDest,[EdDest],[elev(PisoDest1,PisoDest)]):- 
    PisoDest\==PisoDest1,
    elevador(EdDest,LPisos), 
    member(PisoDest1,LPisos), 
    member(PisoDest,LPisos).

segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[cor(PisoAct,PisoSeg)|LOutrasLig]):- 
    (corredor(EdAct,EdSeg,PisoAct,PisoSeg);corredor(EdSeg,EdAct,PisoSeg,PisoAct)),
    segue_pisos(PisoSeg, PisoDest, [EdSeg|LOutrosEd], LOutrasLig).

segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[elev(PisoAct,PisoAct1),cor(PisoAct1,PisoSeg)|LOutrasLig]):-
    (corredor(EdAct,EdSeg,PisoAct1,PisoSeg);corredor(EdSeg,EdAct,PisoSeg,PisoAct1)),
    PisoAct1\==PisoAct,
    elevador(EdAct,LPisos),member(PisoAct,LPisos),member(PisoAct1,LPisos),
    segue_pisos(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig).


/* Algortimo para encontrar o melhor caminho entre dois pisos com menor utilização de elevadores.
Em caso de empate, ou seja, mesmo numero de utilização de elevadores em dois caminhos, opta pelo que tem 
menos corredores no trajecto.
PisoOr - Piso de origem;
PisoDest - Piso de destino;
LLigMelhor - Lista com o melhor trajecto; */
melhor_caminho_pisos(PisoOr,PisoDest,LLigMelhor):-
    findall(LLig,caminho_pisos(PisoOr,PisoDest,_,LLig),LLLig),
    menos_elevadores(LLLig,LLigMelhor,_,_).

menos_elevadores([LLig],LLig,NElev,NCor):-conta(LLig,NElev,NCor).

menos_elevadores([LLig|OutrosLLig],LLigR,NElevR,NCorR):-
    menos_elevadores(OutrosLLig,LLigM,NElev,NCor),
    conta(LLig,NElev1,NCor1),
    (((NElev1<NElev;(NElev1==NElev,NCor1<NCor)),!,
    NElevR is NElev1, NCorR is NCor1,LLigR=LLig);
    (NElevR is NElev,NCorR is NCor,LLigR=LLigM)).

conta([],0,0).
conta([elev(_,_)|L],NElev,NCor):-
    conta(L,NElevL,NCor),NElev is NElevL+1.

conta([cor(_,_)|L],NElev,NCor):-
    conta(L,NElev,NCorL),NCor is NCorL+1.


/* ################################################################### */
/* ########### ALGORITMOS AO NIVEL DO PISO (PEQUENA ESCALA) ########## */
/* ################################################################### */

/* Cria grafo conforme possiveis caminhos (0, 4, 5, 1.4, 1.3, 0.2, 0.3, 0.4, 0.5). Ver circular(N) em BC_trajectos.
ColS- Coluna Adjacente Seguinte;
ColA- Coluna Adjacente Anterior;
Col- Coluna Atual;
LinS- Linha Adjacente Seguinte;
LinA- Linha Adjacente Anterior;
Lin- Linha Atual;
O Assertz são as conexões das entre as celulas (ligacel), TRUE para continuar sem gerar erro caso nao possa transitar para aquela celula. */
cria_grafo(_,0):-!.
cria_grafo(Col,Lin):-
	cria_grafo_lin(Col,Lin),
	Lin1 is Lin-1,
	cria_grafo(Col,Lin1).


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

/*Predicado auxiliar para adicionar conexões com pesos 
cria_conexoes(Col1,Lin1,Col2,Lin2,Peso) :-
    assertz(ligacel(cel(Col1,Lin1),cel(Col2,Lin2),Peso)),
	assertz(edge(cel(Col1,Lin1),cel(Col2,Lin2))).
*/

/* DFS - Pesquisa em profundidade. Pesquisa a partir do nó inicial e segue o caminho até o fim, depois retrocede. 
Deep First Search SEM custo */
dfs(Orig,Dest,Cam):-
	dfs2(Orig,Dest,[Orig],Cam).

dfs2(Dest,Dest,LA,Cam):-reverse(LA,Cam).

dfs2(Act,Dest,LA,Cam):-
	edge(Act,X),
    \+ member(X,LA),
	dfs2(X,Dest,[X|LA],Cam).


/* DFS - Deep First Search COM custo */
dfs_com_custo(Orig,Dest,Cam,Custo):-
	dfs2_com_custo(Orig,Dest,[Orig],Cam,Custo),
    length(Cam,Custo).

dfs2_com_custo(Dest,Dest,LA,Cam,_):-reverse(LA,Cam).

dfs2_com_custo(Act,Dest,LA,Cam,Custo):-   
	edge(Act,X),
    \+ member(X,LA),
	dfs2_com_custo(X,Dest,[X|LA],Cam,Custo).
        %findall((EstX,CaX,[X|LA]), ((ligacel(Act,X,CX); ligacel(X,Act,CX)), \+member(X,LA), estimativa(X,Dest,EstX), CaX is Ca+CX),Novos),


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
vamos pelo caminho mais promissor, tendo em conta o custo/ganho do caminho. 
Breath First Search SEM custo */
bfs(Orig, Dest, Cam):- bfs2(Dest, [[Orig]], Cam).

bfs2(Dest, [[Dest|T]|_], Cam):-reverse([Dest|T], Cam).

bfs2(Dest, [LA|Outros], Cam):-
    LA = [Act|_],
    findall([X|LA], (Dest \== Act, edge(Act, X), \+ member(X, LA)),Novos),
    append(Outros, Novos, Todos),
    bfs2(Dest, Todos, Cam).


/* BFS - Breath First Search COM custo */
bfs_com_custo(Orig, Dest, Cam, Custo):- bfs2_custo(Dest, [[Orig]], Cam,Custo),
    length(Cam,Custo).

bfs2_custo(Dest,(Custo, [[Dest|T]|_]), Cam, Custo):-reverse([Dest|T], Cam).

bfs2_custo(Dest, [LA|Outros], Cam, Custo):-
    LA = [Act|_],
    findall([X|LA], (Dest \== Act, edge(Act, X), \+ member(X, LA)),Novos),
    append(Outros, Novos, Todos),
    bfs2_custo(Dest, Todos, Cam,Custo).

    %findall((EstX,CaX,[X|LA]), ((ligacel(Act,X,CX); ligacel(X,Act,CX)), \+member(X,LA), estimativa(X,Dest,EstX), CaX is Ca+CX),Novos),
    %findall(EstX,([X|LA]), (Dest \== Act, edge(Act, X), \+ member(X, LA)),estimativa(X,Dest,EstX),Novos),
    %append(Outros, Novos, Todos),
    %bfs2_custo(Dest, Todos, Cam,Custo).


/* BEST BFS - Best Breath First Search SEM custo */
bestfs(Orig,Dest,Cam):- bestfs2(Dest,[Orig],Cam).

%condicao final: destino = nó à cabeça do caminho actual
bestfs2(Dest,[Dest|T],Cam):-!,
    /* caminho está invertido */
    reverse([Dest|T],Cam). 

bestfs2(Dest,LA,Cam):- LA=[Act|_], % substituir por member(Act,LA), caso haja cortes nos caminhos
    /* calcular todos os nós adjacentes não visitados e guarda um tuplo com a estimativa e novo caminho */
    findall((EstX,[X|LA]), ((edge(Act,X);edge(X,Act)), \+ member(X,LA), estimativa(X,Dest,EstX)), Novos),
    /*ordena pela estimativa */
    sort(Novos,NovosOrd),
    /* extrai o melhor (header) */
    NovosOrd = [(_,Melhor)|_],
    /* chama-se recursivamente */
    bestfs2(Dest,Melhor,Cam).


/* BEST BFS - Best Breath First Search COM custo */
bestfs(Orig,Dest,Cam,Custo):-
    bestfs2(Dest,(0,[Orig]),Cam,Custo).

bestfs2(Dest,(Custo,[Dest|T]),Cam,Custo):- !,
    reverse([Dest|T],Cam).

bestfs2(Dest,(Ca,LA),Cam,Custo):-
    LA=[Act|_],
    findall((EstX,CaX,[X|LA]), ((ligacel(Act,X,CX); ligacel(X,Act,CX)), \+member(X,LA), estimativa(X,Dest,EstX), CaX is Ca+CX),Novos),
    sort(Novos,NovosOrd),
    NovosOrd = [(_,CM,Melhor)|_],
    bestfs2(Dest,(CM,Melhor),Cam,Custo).


/* Algoritmo para encontrar o caminho mais curto entre dois pontos
Orig - Celula de Origem
Dest - Celula de Destino
Cam - Caminho
Custo - Custo do trajecto.
Devolve uma lista com o caminho percorrido e o repetivo custo associado.*/
aStar(Orig,Dest,Cam,Custo):- 
	aStar2(Dest,[(_,0,[Orig])],Cam,Custo).

/* quando cheagado ao destino, inverte a lista para apresentar o caminho direito. */
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
	/*write('Novos='),write(Novos),nl,*/
	sort(Todos,TodosOrd),
	/*write('TodosOrd='),write(TodosOrd),nl,*/
	aStar2(Dest,TodosOrd,Cam,Custo).

/* calcula o custo para de ir do ponto A ao B.*/
estimativa(cel(X1,Y1),cel(X2,Y2),Estimativa):-
	Estimativa is sqrt((X1-X2)^2+(Y1-Y2)^2).