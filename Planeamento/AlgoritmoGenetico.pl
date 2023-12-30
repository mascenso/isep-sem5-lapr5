:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.
:-dynamic tempo_transicao/3.
:-dynamic lista_tarefas/1.
:-dynamic tarefa/3.


/*Ligacoes HTTP*/
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

% Handler para lidar com requisições HTTP
:- http_handler('/tarefas', tarefas_handler,[]).

% Predicado para iniciar o servidor
server(Port) :-
    http_server(http_dispatch,
                [ port(Port),
                  workers(16)
                ]).

% Handler específico para caminho
tarefas_handler(Request) :-
    cors_enable(Request, [ methods( [get, post, options] ),
        headers( [content_type('application/json'), header('Header-Name')] ),
        methods_allowed([get, post, options])]),
        format('Access-Control-Allow-Origin: *\r\n'),
        format('Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n'),
        format('Access-Control-Allow-Headers: Content-Type, Header-Name\r\n\r\n'),

    http_parameters(Request, [ng(NG,[]),
                              dp(DP,[integer]), 
                              p1(P1,[float]), 
                              p2(P2,[float]), 
                              t(T,[integer]), 
                              av(Av,[integer]), 
                              nestab(NEstab,[])]),
    gera_frontend(NG, DP, P1, P2, T, Av, NEstab, Seq, Temp),
    reply_json(json([sequencia=Seq, tempo=Temp])).

:- consult('BC_RobDroneGo.pl').
:- consult('Percurso_Robots.pl').


gera_frontend(NG,DP,P1,P2,T,Av,NEstab,Seq, Temp):-
    (retract(geracoes(_));true), asserta(geracoes(NG)),
	(retract(populacao(_));true), asserta(populacao(DP)),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), asserta(prob_cruzamento(PC)),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)),
	(retract(tempo_limite(_));true), asserta(tempo_limite(T)),
    (retract(avaliacao_especifica());true), asserta(av_inferior(Av)),    
    (retract(estabilizacao());true), asserta(estabilizacao(NEstab)),!,
	inicializa_tempos_transicao,
	gera_populacao(Pop),
	avalia_populacao(Pop,PopAv),
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	tempo_limite(Tlimite),
	av_inferior(AvEspecifica),
	estabilizacao(NEstab),
	get_time(Tinicial),
	gera_geracao_frontend(0,NG,PopOrd,Tinicial,Tlimite,AvEspecifica,NEstab,0),!,
	final_geracao(Ind*V),!,
    Seq=Ind,
	Temp=V.

% Remove todas as tarefas existentes da Base de Conhecimento
remover_todas_tarefas :-
    retractall(tarefa(_, _, _)).

criar_tarefas([]).

criar_tarefas([[Tarefa, A, B] | Resto]) :-
    assertz(tarefa(Tarefa, A, B)),
    criar_tarefas(Resto).

/* Predicado para inicializar as variaveis necessarias para o algoritmo genético.
NG - Nº de gerações;
DP - Dimensão da População;
P1 - Probabilidade de cruzamento;
P2 - Probabilidade de mutação.
*/
% parameterização
inicializa:-
	write('Numero de novas Geracoes: '),read(NG), 			
    (retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)),
	write('Tempo limite de execucao (S): '), read(T),
	(retract(tempo_limite(_));true), asserta(tempo_limite(T)),
	write('Avaliacao Especifica: '), read(Av),
    (retract(avaliacao_especifica());true), asserta(av_inferior(Av)),    
	write('Numero de Geracoes ate Estabilizacao: '), read(NEstab),nl,
    (retract(estabilizacao());true), asserta(estabilizacao(NEstab)),!.


/* Predicado a ser invocado "à cabeça". Inicializa o algoritmo genético.
Cada elemento da lista é do tipo LT*Av onde LT é uma lista de tarefas (indivíduo), por exemplo [t3,t1,t5,t2,t4], e Av a respetiva avaliação em termos de soma pesada dos atrasos, 
um elemento de PopOrd poderia ser [t3,t1,t5,t2,t4]*16, onde o * é um mero separador.
Nota -> PopOrd - Lista com a geração inicial. */
gera:-
	inicializa,
	inicializa_tempos_transicao,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	tempo_limite(Tlimite),
	av_inferior(AvEspecifica),
	estabilizacao(NEstab),
	get_time(Tinicial),
	gera_geracao(0,NG,PopOrd,Tinicial,Tlimite,AvEspecifica,NEstab,0),
	final_geracao(Ind*V), nl,
	write('Melhor solução: '), write(Ind*V), nl.

/* Predicado para inicializar os tempos de transição entre tarefas */
inicializa_tempos_transicao :-
    retractall(tempo_transicao(_, _, _)),  % Remove versões anteriores do tempo de transição, se existirem
    retractall(lista_tarefas(_)),  % Remove versões anteriores da lista de tarefas, se existirem
    findall(Tarefa, tarefa(Tarefa, _, _), ListaTarefas),
	asserta(lista_tarefas(ListaTarefas)),
    assert_lista_tempos(ListaTarefas, ListaTarefas).

/* Predicado para adicionar os tempos de transição à base de conhecimento */
assert_lista_tempos(_, []).

assert_lista_tempos(Tarefa, [Tarefa1 | Resto]) :-
    valida_tarefa_com_outras(Tarefa1, Resto),
    assert_lista_tempos(Tarefa, Resto).

/* Predicado para validar uma tarefa com todas as outras */
valida_tarefa_com_outras(_, []).

valida_tarefa_com_outras(Tarefa, [OutraTarefa | Resto]) :-
    dif(Tarefa, OutraTarefa),
    not(tempo_transicao(Tarefa, OutraTarefa, _)),
    not(tempo_transicao(OutraTarefa, Tarefa, _)),
	tarefa(Tarefa, _, DestinoT1),
	tarefa(OutraTarefa, OrigemT2, _),
	%Para calcular apenas o custo entre tarefas e nao durante o processamento de tarefas
	localizacao(DestinoT1,PisoOrig, CelulaOrig),
	localizacao(OrigemT2,PisoDest,CelulaDest),
    ((PisoOrig == PisoDest, aStar(CelulaOrig, CelulaDest, _, Custo));
    caminho_pisos_com_custo(PisoOrig, PisoDest, _, _, Custo, _)),
    assert(tempo_transicao(Tarefa, OutraTarefa, Custo)),
    assert(tempo_transicao(OutraTarefa, Tarefa, Custo)),
    valida_tarefa_com_outras(Tarefa, Resto).



/* Cria uma população de indivíduos. Gera a lista de individuos conforme a quantidade de tarefas e tamanho da população. 
Cada indivíduo é representado por uma lista de tarefas de tamanho NumT e é garantido que não haverá indivíduos repetidos.
NumT - quantidade de tarefas; 
TamPop - dimensão da população. */
gera_populacao(Pop):-
	populacao(TamPop),
	lista_tarefas(LTar),
	length(LTar,NumT),
	findall(Tarefa,tarefa(Tarefa,_,_),ListaTarefas),
	gera_populacao(TamPop,ListaTarefas,NumT,Pop).

gera_populacao(0,_,_,[]):-!.

gera_populacao(TamPop,ListaTarefas,NumT,[Ind|Resto]):-
	TamPop1 is TamPop-1,
	gera_populacao(TamPop1,ListaTarefas,NumT,Resto),
	gera_individuo(ListaTarefas,NumT,Ind),
	not(member(Ind,Resto)). %garante que não há indivíduos repetidos. 

gera_populacao(TamPop,ListaTarefas,NumT,L):-
	gera_populacao(TamPop,ListaTarefas,NumT,L).


/* Cria um indivíduo com todas as tarefas, cada tarefa é um gene e o indivíduo corresponde ao cromossoma.
O indivíduo (lista de tarefas) é criado aleatoriamente a partir de uma lista de tarefas disponíveis/aprovadas. */
gera_individuo([G],1,[G]):-!.

gera_individuo(ListaTarefas,NumT,[G|Resto]):-
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaTarefas,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).

retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).


/* Avalia todos os indivíduos da população (cada indivíduo é uma lista com todas as tarefas) de acordo com a soma pesada dos atrasos V  e cria 
uma lista (segundo argumento) com elementos com o formato indivíduo*avaliação.  */
avalia_populacao([],[]).

avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).


/* Predicado de avaliação considerando apenas os custos das deslocações entre as tarefas */
avalia([], 0).

avalia([_], 0).

avalia([T1, T2 | Resto], CustoTotal):-
    (tempo_transicao(T1, T2, Custo);tempo_transicao(T2,T2,Custo)),
    avalia([T2 | Resto], CustoRestante),
    CustoTotal is Custo + CustoRestante.

avalia([T1, T2], CustoTotal):-
    tempo_transicao(T1, T2, CustoTotal).


/* Ordena os elementos da população por ordem crescente de avaliações pela soma pesada dos atrasos.
Usa o bubble sort (bsort) para a ordenação. */
ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).

btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).


/* Gera as próximas gerações da população. Inicia a craição das gerações seguintes, através do cruzamento, mutação e avaliação dos novos indivíduos.
Antes do cruzamento é realizada a permutação aleátoria para minimizar a limitação do cruzamento dos individuos sucessivamente. */
gera_geracao(_,_,Pop,Ti,Tlim,_,_,_):-
	get_time(Tf), 
	TPassado is Tf-Ti,
	TPassado >= Tlim, 
	termina_geracao(Pop), !,
	write('Excedeu o tempo máximo!'),nl.


gera_geracao(_,_,[Ind*V|T1],_,_,AvEspecifica,_,_):-
	V =< AvEspecifica,
	termina_geracao([Ind*V|T1]),!,
	write('Atingiu a avaliação especifica!'),nl.


gera_geracao(_,_,[Ind*V|T1],_,_,_,NEstab,Count):-
	NEstab == Count, 
	termina_geracao([Ind*V|T1]), !,
	write('A população estabilizou!'),nl.


gera_geracao(G,G,Pop,_,_,_,_,_):-
	termina_geracao(Pop), !,
	write('Atingiu o número máximo de novas gerações!'),nl.


gera_geracao(N,G,Pop,Tinicial,Tlimite,AvEspecifica,NEstab,Count):-
	write('Geração '), write(N), write(':'), nl, write(Pop), nl,

	%Para assegurar mais aleatoridade ao cruzamento
	random_permutation(Pop, PopAleatoria), 

	cruzamento(PopAleatoria,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	ordena_populacao(Pop,PopOrd),
	%Para minimizar a presença de repetidos na lista All (T individuos)
	union(PopOrd, NPopOrd, All), 
	ordena_populacao(All, AllOrd),

    length(Pop, Size),
	%Calcula o número de melhores a serem preservados, garantindo que pelo menos 30% sao preservados da população incial
    NumeroMelhoresPreservar is round(Size * 0.3), 

	%Seleciona os melhores indivíduos para preservar na próxima geração
    seleciona_melhores(AllOrd, NumeroMelhoresPreservar, Melhores,Restantes),

	%Multiplica por um numero random entre 0 e 1
    associa_random(Restantes, RestantesComProdutos),
	ordena_populacao(RestantesComProdutos,RestantesComProdutosOrd),

	%Recupera os Av dos individuos
    recupera_avaliacoes(RestantesComProdutosOrd, AvRecuperados), 

	%Vai buscar os N-P individuos da lista dos restantes (AvRecuperados)
	seleciona_NP_restantes(AvRecuperados, Size-NumeroMelhoresPreservar, IndividuosParaProximaGeracao),

	%Inclui os melhores indivíduos na próxima geração, assim como os restantes N-P individuos da lista conjunta da população atual e dos seus descendentes. 
	append(Melhores,IndividuosParaProximaGeracao,NovaGeracao),

	N1 is N+1,

	((compare(D,NovaGeracao,Pop), D == (=), Count1 is Count + 1);Count1 is 0),
    gera_geracao(N1,G,NovaGeracao,Tinicial,Tlimite,AvEspecifica,NEstab,Count1).



/* Predicado para o frontend. Assim não tem "lixo" de prints para lidar na UI*/
gera_geracao_frontend(_,_,Pop,Ti,Tlim,_,_,_):-
	get_time(Tf), 
	TPassado is Tf-Ti,
	TPassado >= Tlim, 
	termina_geracao(Pop).

gera_geracao_frontend(_,_,[Ind*V|T1],_,_,AvEspecifica,_,_):-
	V =< AvEspecifica,
	termina_geracao([Ind*V|T1]).

gera_geracao_frontend(_,_,[Ind*V|T1],_,_,_,NEstab,Count):-
	NEstab == Count, 
	termina_geracao([Ind*V|T1]).

gera_geracao_frontend(G,G,Pop,_,_,_,_,_):-
	termina_geracao(Pop).

gera_geracao_frontend(N,G,Pop,Tinicial,Tlimite,AvEspecifica,NEstab,Count):-
	%Para assegurar mais aleatoridade ao cruzamento
	random_permutation(Pop, PopAleatoria), 

	cruzamento(PopAleatoria,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	ordena_populacao(Pop,PopOrd),
	%Para minimizar a presença de repetidos na lista All (T individuos)
	union(PopOrd, NPopOrd, All), 
	ordena_populacao(All, AllOrd),

    length(Pop, Size),
	%Calcula o número de melhores a serem preservados, garantindo que pelo menos 30% sao preservados da população incial
    NumeroMelhoresPreservar is round(Size * 0.3), 

	%Seleciona os melhores indivíduos para preservar na próxima geração
    seleciona_melhores(AllOrd, NumeroMelhoresPreservar, Melhores,Restantes),

	%Multiplica por um numero random entre 0 e 1
    associa_random(Restantes, RestantesComProdutos),
	ordena_populacao(RestantesComProdutos,RestantesComProdutosOrd),

	%Recupera os Av dos individuos
    recupera_avaliacoes(RestantesComProdutosOrd, AvRecuperados), 

	%Vai buscar os N-P individuos da lista dos restantes (AvRecuperados)
	seleciona_NP_restantes(AvRecuperados, Size-NumeroMelhoresPreservar, IndividuosParaProximaGeracao),

	%Inclui os melhores indivíduos na próxima geração, assim como os restantes N-P individuos da lista conjunta da população atual e dos seus descendentes. 
	append(Melhores,IndividuosParaProximaGeracao,NovaGeracao),

	N1 is N+1,

	((compare(D,NovaGeracao,Pop), D == (=), Count1 is Count + 1);Count1 is 0),
    gera_geracao_frontend(N1,G,NovaGeracao,Tinicial,Tlimite,AvEspecifica,NEstab,Count1).




/* Predicado para finalizar o predicado*/	
termina_geracao([Ind*V|_]):-
    (retract(final_geracao());true), asserta(final_geracao(Ind*V)),!.


/* Predicado para muitliplicar um numero random entre 0 e 1 pela Av dos individuos. */
associa_random([], []).
associa_random([Ind*Av|Restantes], [Power-Ind*Av|RestantesComPotencia]):-
    random(0.0, 1.0, Random),
    Power is Av * Random,
    associa_random(Restantes, RestantesComPotencia).


/* Predicado para recuperar a Av original dos individuos . */
recupera_avaliacoes([], []).

recupera_avaliacoes([_-Ind*Av|RestantesComPotencia], [Ind*Av|RestantesAvaliacoes]):-
    recupera_avaliacoes(RestantesComPotencia, RestantesAvaliacoes).


/* Predicado para ir buscar os N - P restantes individuos da lista dos restantes resultantes 
aquando da selecção dos melhores entre a população atual e dos seus descendentes. */
seleciona_NP_restantes([], _, []).

seleciona_NP_restantes(_, 0, []).

seleciona_NP_restantes([Individuo*Av|Resto], N, [Individuo*Av|Selecionados]) :-
    N > 0,
    N1 is N - 1,
    seleciona_NP_restantes(Resto, N1, Selecionados).

seleciona_NP_restantes([_|Resto], N, Selecionados) :-
    N > 0,
    seleciona_NP_restantes(Resto, N, Selecionados).


/* Seleciona os N melhores indivíduos da lista ordenada daquela geração. */
seleciona_melhores([Ind*V|Resto], N, Melhores, Restantes) :-
    N > 0,
    N1 is N - 1,
    seleciona_melhores(Resto, N1, MelhoresRestantes, Restantes),
    Melhores = [Ind*V | MelhoresRestantes].

seleciona_melhores(Resto, 0, [], Resto).  % Quando N chega a 0, a lista Melhores está completa, os restantes são os Restantes.


/* Geração dos pontos de cruzamento P1 (onde começa o corte) e P2 (onde acaba o corte), 
por exemplo se P1 for 2 e P2 for 4 os pontos de corte serão entre o 1º e 2º gene e entre o 4º e 5º gene.
Notar que tal como está implementado não há cortes que fiquem apenas com 1 gene a meio, por causa do P11 ser diferente do P21 */
gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	lista_tarefas(LTar),
	length(LTar,N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	dif(P11,P21),!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).

gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).


/* O cruzamento realizado sobre indivíduos sucessivos 2 a 2 da população. Para saber se se realiza o cruzamento gera-se um nº aleatório entre 0 e 1, 
e compara-se com a probabilidade de cruzamento parametrizada, se for inferior faz-se o cruzamento */
cruzamento([],[]).

cruzamento([Ind*_],[Ind]).

cruzamento([Ind1*_,Ind2*_|Resto],[NInd1,NInd2|Resto1]):-
	gerar_pontos_cruzamento(P1,P2),
	prob_cruzamento(Pcruz),random(0.0,1.0,Pc),
	((Pc =< Pcruz,!, cruzar(Ind1,Ind2,P1,P2,NInd1), cruzar(Ind2,Ind1,P1,P2,NInd2)) ;
	(NInd1=Ind1,NInd2=Ind2)),
	cruzamento(Resto,Resto1).


/* Predicados auxiliares para fazer o cruzamento order crossover, que é o adequado para o sequenciamento de tarefas */
preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


/*Predicados auxiliares para fazer o cruzamento order crossover, que é o adequado para o sequenciamento de tarefas */
sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	lista_tarefas(LTar),
	length(LTar,N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	lista_tarefas(LTar),
	length(LTar,T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	lista_tarefas(LTar),
	length(LTar,NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).

eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).


/* Tentativa de mutação sobre cada indivíduo da população. Para saber se se realiza a mutação gera-se um nº aleatório entre 0 e 1
 e compara-se com a probabilidade de mutação parametrizada, se for inferior faz-se a mutação */
mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).


