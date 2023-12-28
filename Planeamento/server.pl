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


% Handler específico para as tarefas
tarefas_handler(Request) :-
    cors_enable(Request, [ methods( [get, post, options] ),
        headers( [content_type('application/json'), header('Header-Name')] ),
        methods_allowed([get, post, options])]),
        format('Access-Control-Allow-Origin: *\r\n'),
        format('Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n'),
        format('Access-Control-Allow-Headers: Content-Type, Header-Name\r\n\r\n'),

    http_parameters(Request, [ng(NG,[]),
                              dp(DP,[]), 
                              p1(P1,[p1]), 
                              p2(P2,[]), 
                              t(T,[]), 
                              av(Av,[]), 
                              nestab(NEstab,[])]),
    gera_frontend(NG, DP, P1, P2, T, Av, NEstab, F),
    reply_json(json([tarefas=F])).

stop_server_before_exit :-
    stop_server(8081),
    true.

at_halt(stop_server_before_exit).



