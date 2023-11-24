:-dynamic ligacel/3.
:-dynamic edge/2.

:- consult('AlgoritmosGenericos.pl').
:- consult('BC_RobDroneGo.pl').


/* Cria grafo para o piso indicado tendo em considelaraçao o mapa do piso. */
cria_grafo_piso(Piso):-
    floor_map(Piso, Map),
    cria_grafo_mapa(Map).

cria_grafo_mapa(Map):-
    length(Map, NumLinhas),
    length(Map, NumColunas),
    cria_grafo(NumLinhas, NumColunas,Map).


/* Cria grafo conforme possiveis caminhos especializado para receber uma matriz dos pisos.
ColS- Coluna Adjacente Seguinte;
ColA- Coluna Adjacente Anterior;
Col- Coluna Atual;
LinS- Linha Adjacente Seguinte;
LinA- Linha Adjacente Anterior;
Lin- Linha Atual;
O Assertz são as conexões das entre as celulas (ligacel), TRUE para continuar sem gerar erro caso nao possa transitar para aquela celula. */
cria_grafo(_,0,_):-!.
cria_grafo(Col,Lin,Map):-
	cria_grafo_lin(Col,Lin,Map),
	Lin1 is Lin-1,
	cria_grafo(Col,Lin1,Map).


cria_grafo_lin(0,_,_):-!.


cria_grafo_lin(Col,Lin, Map):-
	map_value(Col,Lin,Map,V),
    write('v='),write(V),nl,
	not(parede(V)),!,
	ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,
	((map_value(ColS,Lin,Map,V),circular(V),(cria_conexoes(Col,Lin,ColS,Lin,1));true)), %M. Horizontal
	((map_value(ColA,Lin,Map,V),circular(V),(cria_conexoes(Col,Lin,ColA,Lin,1));true)), %M. Horizontal
	((map_value(Col,LinS,Map,V),circular(V),(cria_conexoes(Col,Lin,Col,LinS,1));true)), %M. Vertical
	((map_value(Col,LinA,Map,V),circular(V),(cria_conexoes(Col,Lin,Col,LinA,1));true)), %M. Vertical
	((map_value(ColS,LinS,Map,V),circular(V),(cria_conexoes(Col,Lin,ColS,LinS,sqrt(2)));true)), %M. Diagonal
	((map_value(ColA,LinS,Map,V),circular(V),(cria_conexoes(Col,Lin,ColA,LinS,sqrt(2)));true)), %M. Diagonal
	((map_value(ColS,LinA,Map,V),circular(V),(cria_conexoes(Col,Lin,ColS,LinA,sqrt(2)));true)), %M. Diagonal
	((map_value(ColA,LinA,Map,V),circular(V),(cria_conexoes(Col,Lin,ColA,LinA,sqrt(2)));true)), %M. Diagonal
	Col1 is Col-1,
	cria_grafo_lin(Col1,Lin,Map).


cria_grafo_lin(Col,Lin,Map):-Col1 is Col-1,cria_grafo_lin(Col1,Lin,Map).

map_value(Col,Lin,Map,V):- 
    nth1(Col, Map, Coluna),
    nth1(Lin, Coluna, V).

/*Predicado auxiliar para adicionar conexões com pesos */
cria_conexoes(Col1,Lin1,Col2,Lin2,Peso) :-
    assertz(ligacel(cel(Col1,Lin1),cel(Col2,Lin2),Peso)),
	assertz(edge(cel(Col1,Lin1),cel(Col2,Lin2))).

/* Predicado para mostrar as conexões criadas -> testar grafo */
mostra_conexoes2 :-
    ligacel(Cel1, Cel2, Peso),
    write('Conexão de '), write(Cel1), write(' para '), write(Cel2), write(' com peso '), write(Peso), nl,
    fail.
mostra_conexoes.


/* Algoritmo para encontrar todos os caminhos entre dois pisos,PisoOr e PisoDest.
Devolve uma lista de edificios percorridos e uma lista das ligaçoes (elevadores e|ou corredores)
PisoOr - Piso de origem;
PisoDest - Piso de destino;
LCam - Lista de caminhos percorrido;
LLig - Lista de ligaçoes percorridas;
Custo - Custo associado ao percuros;
*/

caminho_pisos_com_custo(PisoOr, PisoDest, LCam, LLig, CustoTotal):-
    pisos(EdOr, LPisosOr),
    member(PisoOr, LPisosOr),
    pisos(EdDest, LPisosDest),
    member(PisoDest, LPisosDest), 
    caminho_edificios(EdOr, EdDest, LCam),
    segue_pisos(PisoOr,PisoDest,LCam,LLig),
    percorre_primeiro_lista(LLig, PisoOr, CustoTotal). 
    %write('CustoTotal= '),write(CustoTotal),nl.

%este algoritmo é para calcular a distancia entre a posiçao inicial do robot com a de destino na primeira interaçao. Se a lista estiver vazia, não avança.
percorre_primeiro_lista([], _, _,0).

percorre_primeiro_lista([elev(PisoOr, _) | Resto], PisoOr, CustoTotal):-
        pos_init(PisoOr,Orig),
        elev_pos(PisoOr, CDestino),
        aStar(Orig, CDestino, _, Custo),
        write('Custo = '),write(Custo),nl,
        NovoCustoTotal is CustoTotal + Custo,
        percorre_lista(Resto, CDestino, NovoCustoTotal).


percorre_primeiro_lista([cor(PisoOr, PisoDest) | Resto], PisoOr, CustoTotal):-
        pos_init(PisoOr,Orig),
        (passag_pos(PisoOr,PisoDest,CDestino); passag_pos(PisoDest,PisoOr, CDestino)),
        aStar(Orig, CDestino, _, Custo),
        write('Custo = '),write(Custo),nl,
        NovoCustoTotal is CustoTotal + Custo,
        percorre_lista(Resto, CDestino ,NovoCustoTotal).

%este algoritmo é para calcular a distancia entre a posiçao anterior do robot com a proxima. Se chegou ao destino, para.
percorre_lista([], _,0).

percorre_lista([elev(_, _) | Resto], Destino, CustoTotal):-
    percorre_lista(Resto, Destino, CustoTotal).

percorre_lista([cor(PisoOr, PisoDest) | Resto], COrig,CustoTotal):-
    (passag_pos(PisoOr,PisoDest,CDestino); passag_pos(PisoDest,PisoOr, CDestino)),
    aStar(COrig, CDestino, _, Custo),
    write('Custo = '),write(Custo),nl,
    NovoCustoTotal is CustoTotal + Custo,
    percorre_lista(Resto, CDestino, NovoCustoTotal).
