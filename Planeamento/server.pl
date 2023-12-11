/*Ligacoes HTTP*/
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).

:- consult('Algoritmos.pl').
:- consult('BC_RobDroneGo.pl').
:- consult('Percurso_Robots.pl').
:- consult('AlgoritmoGenetico.pl').

:- set_setting(http:cors, [*]).

:-cria_grafo_piso(a1).
:-cria_grafo_piso(a2).
:-cria_grafo_piso(b1).
:-cria_grafo_piso(c3).

% Handler para lidar com requisições HTTP
:- http_handler('/caminho', caminho_handler, []).

% Predicado para iniciar o servidor
start_server(Port) :-
    
    http_server(http_dispatch, [port(Port)]).

% Predicado para parar o servidor
stop_server(Port) :-
    http_stop_server(Port,[]).

% Handler específico para caminho
caminho_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    http_parameters(Request, [pisoOrigem(PisoOr, []),
                              pisoDestino(PisoDest, [])]),
    caminho_pisos_com_custo(PisoOr, PisoDest, LCam, LLig, CustoTotal),
    reply_json(json([caminho=LCam, custo=CustoTotal])).

stop_server_before_exit :-
    stop_server(8081),
    true.

at_halt(stop_server_before_exit).
