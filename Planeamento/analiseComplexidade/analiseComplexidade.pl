:- dynamic tempo_transicao/3.
:- dynamic tarefa/2.
:- dynamic best_sequence/2.

% Gera uma tarefa no formato (Id, tempoProcessamento)
gerar_tarefa(Id, TempoProcessamento) :-
    atomic_concat(t, Id, IdAtom),
    random_between(1, 20, TempoProcessamento),
    assertz(tarefa(IdAtom, TempoProcessamento)).

% Gera uma lista de tarefas aleatórias com base no número fornecido no formato (ID, tempoProcessamento)
gerar_tarefas_aleatorias(NumeroTarefas, Tarefas) :-
    gerar_tarefas_aleatorias_aux(NumeroTarefas, 1, [], Tarefas).

gerar_tarefas_aleatorias_aux(0, _, Tarefas, Tarefas).
gerar_tarefas_aleatorias_aux(N, IdInicial, TarefasParciais, Tarefas) :-
    N > 0,
    NovoId is IdInicial + 1,
    gerar_tarefa(IdInicial, Tempo),
    atomic_concat(t, IdInicial, IdAtom),                                            %para o nome da tarefa na lista ficar igual ao id da tarefa "t1,t2 etc"
    append(TarefasParciais, [(IdAtom, Tempo)], TarefasAtualizadas),
    N1 is N - 1,
    gerar_tarefas_aleatorias_aux(N1, NovoId, TarefasAtualizadas, Tarefas).

% Gera tempos de transição entre todas as tarefas consecutivas
gerar_tempos_transicao([], []).
gerar_tempos_transicao([(_, _) | []], []).
gerar_tempos_transicao([(T1, _) | Resto], TemposTransicao) :-
    gerar_tempos_para_tarefa(T1, Resto, TempoTransicao),
    gerar_tempos_transicao(Resto, TemposTransicoesResto),
    append(TempoTransicao, TemposTransicoesResto, TemposTransicao).

% Gera tempos de transição para uma tarefa específica
gerar_tempos_para_tarefa(_, [], []).
gerar_tempos_para_tarefa(T1, [(T2, _) | Resto], [TempoTransicao | TempoTransicoesResto]) :-
    gerar_tempo_transicao(T1, T2, TempoTransicao),
    gerar_tempos_para_tarefa(T1, Resto, TempoTransicoesResto).

% Gera tempo de transição entre duas tarefas específicas
gerar_tempo_transicao(TarefaAtual, ProximaTarefa, TempoTransicao) :-
    random_between(1, 10, TempoTransicao), % Intervalo de tempo de transição aleatório (pode ser ajustado)
    assertz(tempo_transicao(TarefaAtual, ProximaTarefa, TempoTransicao)).


% Gera tarefas e todas as TempoTransicoes
gerar_tarefas_e_transicoes(NumeroTarefas, Tarefas, TemposTransicao) :-
    gerar_tarefas_aleatorias(NumeroTarefas, Tarefas),
    gerar_tempos_transicao(Tarefas, TemposTransicao).

% Todas as permutacoes
allPermutation(List, Permutacoes):-
    findall(Perm, permutation(List,Perm),Permutacoes).


% Funcao auxiliar para validar transicoes
transicao(T1, T2, Tempo) :-
    tempo_transicao(T1, T2, Tempo).
transicao(T1, T2, Tempo) :-
    tempo_transicao(T2, T1, Tempo).


% Predicado para sequenciar tarefas, devolve a sequencia e o tempo total das transicoes
sequenciar_as_tarefas(Sequencias) :-
    findall((Sequencia, TempoTotal), sequenciar_as_tarefas_aux((Sequencia, TempoTotal)), Sequencias).

% Predicado auxiliar para gerar sequências de tarefas
sequenciar_as_tarefas_aux((Sequencia, TempoTotal)) :-
    findall((Id, Tempo), tarefa(Id, Tempo), Tarefas),
    permutation(Tarefas, Sequencia),
    tempo_total(Sequencia, TempoTotal),
    validar_sequencia(Sequencia).

% Predicado para verificar se uma sequência de tarefas é válida, tem de existir na base de conhecimento
validar_sequencia([]).
validar_sequencia([_]).
validar_sequencia([(T1, _), (T2, _) | Resto]) :-
    transicao(T1, T2, _),
    validar_sequencia([(T2, _) | Resto]).

% Calculo do tempo total de uma sequencia de tarefas, apenas calcula tempo entre tarefas e nao tempo da tarefa
tempo_total([], 0).
tempo_total([(_, _)], 0).
tempo_total([(T1, _), (T2, _) | Resto], TempoTotal) :-
    tempo_total([(T2, _) | Resto], TempoResto),
    transicao(T1, T2, TempoTransicao),
    TempoTotal is TempoResto + TempoTransicao.


sequenciar_as_tarefas_sem_stack(MelhorSequencia, MenorTempo) :-
    asserta(best_sequence(_,10000)),
    sequenciar_as_tarefas_aux((Seq, Tempo)),
    atualizar_best_sequence(Seq,Tempo),
    fail.
    

melhor_sequencia_de_tarefas(MelhorSequencia, MenorTempo) :-
    get_time(TempoInicial),
    (sequenciar_as_tarefas_sem_stack(MelhorSequencia, MenorTempo) ; true),
    get_time(TempoFinal),
    TempoExecucao is TempoFinal - TempoInicial,
    write('Melhor Sequencia: '), write(best_sequence(S,T)), nl,
    write('Tempo de execução: '), write(TempoExecucao), nl.



atualizar_best_sequence(Seq,Tempo) :-
    best_sequence(_,N),
    Tempo < N,
    retract(best_sequence(_,_)),
    asserta(best_sequence(Seq,Tempo)).
    



