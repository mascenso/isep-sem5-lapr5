/* Ficheiro para arranque do planeamento. */
:- dynamic edificio/1.
:- dynamic piso/1.

:- consult('BC_trajectos.pl').
:- consult('Algoritmos.pl').
:- consult('Percurso_Robots.pl').

main :-
    write('Bem-vindo!'), nl,
    menu.

menu :-
    write('Escolha uma opção:'), nl,
    write('1 - Ver caminhos entre dois edifícios.'), nl,
    write('2 - Ver caminhos entre dois pisos.'), nl,
    write('3 - Ver trajecto do robot.'), nl,
    write('0 - Sair.'), nl,
    read(Opcao),
    menu_secundario(Opcao),
    menu.

menu_secundario(1):-
    write('Escolha uma opção:'), nl,
    write('1 - Ver todos os caminhos entre dois edifícios.'), nl,
    write('2 - Ver um caminho entre dois edifícios.'), nl,
    write('3 - Ver o caminho mais curto entre dois edifícios.'), nl,
    write('4 - Ver o caminho que passa em menos edificios entre dois edifícios.'), nl,
    write('9 - Voltar atrás.'), nl,
    write('0 - Sair.'), nl,
    read(Opcao),
    processar_opcao(Opcao),
    menu_secundario.

menu_secundario(2):-
    write('Escolha uma opção:'), nl,
    write('1 - Ver todos os caminhos entre dois pisos.'), nl,
    write('2 - Ver um caminho entre dois pisos.'), nl,
    write('3 - Ver o caminho mais curto entre dois pisos.'), nl,
    write('4 - Ver o caminho com menos utilizações de elevadores entre dois pisos.'), nl,
    write('9 - Voltar atrás.'), nl,
    write('0 - Sair.'), nl,
    read(Opcao),
    processar_opcao(Opcao),
    menu_secundario.

menu_secundario(2):-
    write('Escolha uma opção:'), nl,
    write('1 - Ver todos os caminhos entre dois pisos.'), nl,
    write('2 - Ver um caminho entre dois pisos.'), nl,
    write('3 - Ver o caminho mais curto entre dois pisos.'), nl,
    write('4 - Ver o caminho com menos utilizações de elevadores entre dois pisos.'), nl,
    write('9 - Voltar atrás.'), nl,
    write('0 - Sair.'), nl,
    read(Opcao),
    processar_opcao(Opcao),
    menu_secundario.


menu_secundario(_) :-
    write('Opção inválida. Tente novamente.'), nl.

processar_opcao(1) :-
    write('Digite o edifício de origem: '),
    read(EdificioOrigem),
    write('Digite o edifício de destino: '),
    read(EdificioDestino),
    todos_caminhos_edificios(EdificioOrigem, EdificioDestino, ListaDeCaminhos),
    write('Caminhos encontrados: '), write(ListaDeCaminhos), nl.

processar_opcao(2) :-
    % Adicione aqui a lógica para um caminho entre edifícios específicos.

processar_opcao(3) :-
    % Adicione aqui a lógica para um caminho entre pisos específicos.

processar_opcao(4) :-
    % Adicione aqui a lógica para criar um grafo.

processar_opcao(5) :-
    % Adicione aqui a lógica para o percurso mais curto do robô.

processar_opcao(0) :-
    write('Até logo!'), nl,
    halt.

processar_opcao(_) :-
    write('Opção inválida. Tente novamente.'), nl.



