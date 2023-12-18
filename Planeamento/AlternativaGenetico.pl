:- use_module(library(lists)).

:- consult('BC_RobDroneGo.pl').
:- consult('AlgoritmoGenetico.pl').


/* Predicado para gerar todas as permutações das tarefas */
gera_permutacoes(ListaTarefas, TodasPermutacoes) :-
    findall(Permutacao, permutation(ListaTarefas, Permutacao), TodasPermutacoes).


/* Predicado para avaliar uma permutação de tarefas */
avalia_permutacao(Permutacao, Avaliacao) :-
    avalia(Permutacao, Avaliacao).


/* Predicado para encontrar a melhor solução dentre todas as permutações */
melhor_solucao(ListaTarefas, MelhorPermutacao) :-
    gera_permutacoes(ListaTarefas, TodasPermutacoes),
    melhor_solucao_aux(TodasPermutacoes, ListaTarefas, 9999, _, MelhorPermutacao). % Valor inicial alto para a melhor avaliação


melhor_solucao_aux([], _, MelhorAvaliacao, MelhorPermutacao, MelhorPermutacao) :-
    MelhorAvaliacao \= 9999. % Verifica se encontrou uma solução válida.


melhor_solucao_aux([Permutacao | Resto], ListaTarefas, MelhorAvaliacaoAtual, _, MelhorPermutacao) :-
    avalia_permutacao(Permutacao, Avaliacao),
    Avaliacao < MelhorAvaliacaoAtual,
    melhor_solucao_aux(Resto, ListaTarefas, Avaliacao, Permutacao, MelhorPermutacao).


melhor_solucao_aux([_ | Resto], ListaTarefas, MelhorAvaliacaoAtual, MelhorPermutacaoAtual, MelhorPermutacao) :-
    melhor_solucao_aux(Resto, ListaTarefas, MelhorAvaliacaoAtual, MelhorPermutacaoAtual, MelhorPermutacao).

