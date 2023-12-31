:- module(server,
      [ server/1            % ?Port
      ]).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_dyn_workers)).
:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).
:- use_module(library(http/http_cors)).

:- consult('Algoritmos.pl').
:- consult('BC_RobDroneGo.pl').
:- consult('Percurso_Robots.pl').
:- consult('AlgoritmoGenetico.pl').

:-cria_grafo_piso(a1).
:-cria_grafo_piso(a2).
:-cria_grafo_piso(b1).
:-cria_grafo_piso(c3).

% Handler para lidar com solicitações OPTIONS
:- http_handler('/caminho', caminho_handler, [methods([get, post, options])]).
:- http_handler('/tarefas', tarefas_handler, [methods([get, post, options])]).


server(Port) :-
    http_server(http_dispatch,
                [ port(Port),
                  workers(16)
                ]).


% Predicado para parar o servidor
stop_server(Port) :-
    http_stop_server(Port,[]).

% Handler específico para caminho
caminho_handler(Request) :-
                cors_enable(Request, [ methods( [get, post, options] ),
                                       headers( [content_type('application/json'), header('Header-Name')] ),
                                       methods_allowed([get, post, options])]),
                format('Access-Control-Allow-Origin: *\r\n'),
                format('Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n'),
                format('Access-Control-Allow-Headers: Content-Type, Header-Name\r\n\r\n'),
    http_parameters(Request, [pisoOrigem(PisoOr, []),
                              pisoDestino(PisoDest, [])]),
    caminho_pisos_com_custo(PisoOr, PisoDest, LCam, LLig, CustoTotal, Cel),
    reply_json(json([caminho=LCam, custo=CustoTotal])).

% Handler específico para lista de tarefas
listaTarefas_handler(Request) :-
                cors_enable(Request, [ methods( [get, post, options] ),
                                       headers( [content_type('application/json'), header('Header-Name')] ),
                                       methods_allowed([get, post, options])]),
                format('Access-Control-Allow-Origin: *\r\n'),
                format('Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n'),
                format('Access-Control-Allow-Headers: Content-Type, Header-Name\r\n\r\n'),
  % http_parameters(Request, [tarefa(T, []),
							origX(A, [integer]),
							origY(B, [integer]),
							pisoOrigem(C, []),
							destX(D, [integer]),
							destY(E, [integer]),
                            pisoDestino(F, [])]),
		%http_read_json_dict(Request,Dados),
		%adicionar_tarefa(Dados.tarefa, Dados.origX, Dados.origY, Dados.pisoOrigem, Dados.destX, Dados.destY, Dados.F),
		  http_parameters(Request, [tarefa(T, []),
                              origX(OrigX, []),
                              origY(OrigY, []),
                              pisoOrigem(PisoOrigem, []),
                              destX(DestX, []),
                              destY(DestY, []),
                              pisoDestino(PisoDestino, [])]),

    % Converter valores que deveriam ser números para inteiros
    term_to_atom(OrigX, X), % Converte OrigX para um número
    term_to_atom(OrigY, Y), % Converte OrigY para um número
    term_to_atom(DestX, DX), % Converte DestX para um número
    term_to_atom(DestY, DY), % Converte DestY para um número
        adicionar_tarefa(T, X, Y, C, DX, DY, F),
		obter_tarefas(Lista),
    	reply_json(json([lista=Lista])).

% Handler específico para as tarefas
tarefas_handler(Request) :-
    cors_enable(Request, [ methods( [get, post, options] ),
        headers( [content_type('application/json'), header('Header-Name')] ),
        methods_allowed([get, post, options])]),
        format('Access-Control-Allow-Origin: *\r\n'),
        format('Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n'),
        format('Access-Control-Allow-Headers: Content-Type, Header-Name\r\n\r\n'),

    http_parameters(Request, [ng(NG,[]),
                              dp(DP,[integer]), 
                              p1(P1,[integer]), 
                              p2(P2,[integer]), 
                              t(T,[integer]), 
                              av(Av,[integer]), 
                              nestab(NEstab,[])]),
    %term_to_atom(X,LTasks),

   %parse_tasks(X,Tasks),
    gera_frontend(NG, DP, P1, P2, T, Av, NEstab, Seq, Temp),
    %http_read_json_dict(Request,Dados),
    %parse_tasks(Dados.LTasks,Tasks),
    %gera_frontend(Dados.LTasks,Dados.NG, Dados.DP, Dados.P1, Dados.P2, Dados.T, Dados.Av, Dados.NEstab, Seq, Temp),
    reply_json(json([sequencia=Seq, tempo=Temp])).

stop_server_before_exit :-
    stop_server(8081),
    true.

at_halt(stop_server_before_exit).



