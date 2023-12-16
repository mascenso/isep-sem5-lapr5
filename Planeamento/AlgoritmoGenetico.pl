:-dynamic geracoes/1.
:-dynamic populacao/1.
:-dynamic prob_cruzamento/1.
:-dynamic prob_mutacao/1.


% tarefa(Id,TempoProcessamento,TempConc,PesoPenalizacao).
tarefa(t1,2,5,1).
tarefa(t2,4,7,6).
tarefa(t3,1,11,2).
tarefa(t4,3,9,3).
tarefa(t5,3,8,3).
%tarefa(t6,4,5,2).


% tarefas(NTarefas).
tarefas(5).

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
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).


/* Predicado a ser invocado "à cabeça". Inicializa o algoritmo genético.
Cada elemento da lista é do tipo LT*Av onde LT é uma lista de tarefas (indivíduo), por exemplo [t3,t1,t5,t2,t4], e Av a respetiva avaliação em termos de soma pesada dos atrasos, 
um elemento de PopOrd poderia ser [t3,t1,t5,t2,t4]*16, onde o * é um mero separador.
Nota -> PopOrd - Lista com a geração inicial.
*/
gera:-
	inicializa,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(0,NG,PopOrd).

	

/* Cria uma população de indivíduos. Gera a lista de individuos conforme a quantidade de tarefas e tamanho da população. 
Cada indivíduo é representado por uma lista de tarefas de tamanho NumT e é garantido que não haverá indivíduos repetidos.
NumT - quantidade de tarefas; 
TamPop - dimensão da população. 
*/
gera_populacao(Pop):-
	populacao(TamPop),
	tarefas(NumT),
	findall(Tarefa,tarefa(Tarefa,_,_,_),ListaTarefas),nl,
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
uma lista (segundo argumento) com elementos com o formato indivíduo*avaliação. */
avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

avalia(Seq,V):-
	avalia(Seq,0,V).

avalia([],_,0).
avalia([T|Resto],Inst,V):-
	tarefa(T,Dur,Prazo,Pen),
	InstFim is Inst+Dur,
	avalia(Resto,InstFim,VResto),
	((InstFim =< Prazo,!, VT is 0) ; (VT is (InstFim-Prazo)*Pen)),
	V is VT+VResto.


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
gera_geracao(G,G,Pop):-!,
	write('Geração '), write(G), write(':'), nl, write(Pop), nl.

gera_geracao(N,G,Pop):-
	write('Geração '), write(N), write(':'), nl, write(Pop), nl,
	ordena_populacao(Pop,PopOrd),
	random_permutation(Pop, PopAleatoria), %Permutação aleatória da população
	cruzamento(PopAleatoria,NPop1),
	mutacao(NPop1,NPop),
	avalia_populacao(NPop,NPopAv),
	ordena_populacao(NPopAv,NPopOrd),
	union(PopOrd, NPopOrd, All), % para minimizar a presença de repetidos na lista All
	ordena_populacao(All, AllOrd),

	%Seleciona os melhores indivíduos para preservar na próxima geração
    NumeroMelhoresPreservar = 2, % Pensar melhor na forma como passar este valor para aqui.
    seleciona_melhores(AllOrd, NumeroMelhoresPreservar, Melhores),nl,
	length(Pop, Size),
	% Inclui os melhores indivíduos na próxima geração
	inclui_melhores(Melhores, NPopOrd,Size, NovaGeracao),
	N1 is N+1,
	gera_geracao(N1,G,NovaGeracao).


/* Seleciona os N melhores indivíduos da lista ordenada daquela geração. */
seleciona_melhores(_, 0, []).

seleciona_melhores([Ind*V|Resto], N, [Ind*V|Melhores]) :-
    N > 0,
    N1 is N - 1,
    seleciona_melhores(Resto, N1, Melhores).


/* Inclui os melhores indivíduos na próxima geração. Não permite duplicados na Próxima Geração. */
inclui_melhores([], PopulacaoAtual, _, PopulacaoAtual).

inclui_melhores([Melhor|Melhores], PopulacaoAtual, PopSize, ProximaGeracao) :-
    not(member(Melhor, PopulacaoAtual)),
    length(PopulacaoAtual, CurrentSize),
    NewSize is CurrentSize + 1,
    NewSize =< PopSize,
    inclui_melhores(Melhores, [Melhor|PopulacaoAtual], PopSize, ProximaGeracao).

inclui_melhores([Melhor|Melhores], PopulacaoAtual, PopSize, ProximaGeracao) :-
    member(Melhor, PopulacaoAtual),
	length(PopulacaoAtual, CurrentSize),
    NewSize is CurrentSize + 1,
    NewSize =< PopSize,
    inclui_melhores(Melhores, PopulacaoAtual, PopSize, ProximaGeracao).

inclui_melhores(_, PopulacaoAtual, _, PopulacaoAtual).


/* Geração dos pontos de cruzamento P1 (onde começa o corte) e P2 (onde acaba o corte), 
por exemplo se P1 for 2 e P2 for 4 os pontos de corte serão entre o 1º e 2º gene e entre o 4º e 5º gene.
Notar que tal como está implementado não há cortes que fiquem apenas com 1 gene a meio, por causa do P11 ser diferente do P21 */
gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	tarefas(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
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
	tarefas(N),
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
	tarefas(T),
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
	tarefas(NumT),
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