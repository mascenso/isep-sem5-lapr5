/* ## ALGORITMOS PARA O CALCULO DOS CAMINHOS/TRAJECTOS DO ROBDRONEGO ENTRE EDIFICIOS E PISOS. GRANULARIDADE GRANDE ## */

/* ################################################################### */
/* ################# ALGORTIMOS AO NIVEL DO EDIFICIO ################# */
/* ################################################################### */

/* Algoritmo para encontrar um caminho entre dois edificios, EdOr e EdDest.
Devolve uma lista de edificios percorridos
EdOr -Edificio Origem;
EdDest - Edifcio Destino; 
Lcam - Lista de edificios percorridos; */
caminho_edificios(EdOr,EdDest,LCam):- caminho_edificios2(EdOr,EdDest,[EdOr],LCam).

%Condição de paragem. Se o Edx=EdDest é sinal que chegou ao destino. 
%É necessário inverter a lista para aparecer pela ordem de passagem.
caminho_edificios2(EdX,EdX,LEdInv,LCam):-!, reverse(LEdInv,LCam).

caminho_edificios2(EdAct,EdDest,LEdPassou,LCam):- 
    %verfica se há ligação entre os edificios e um edificio intermédio.
    (liga(EdAct,EdInt);liga(EdInt,EdAct)), 
    %verfica se já passou pelo edificio intermédio
    \+member(EdInt,LEdPassou),
    %recursividade para esgotar as opções.
    caminho_edificios2(EdInt,EdDest,[EdInt|LEdPassou],LCam).


/* Algoritmo para encontrar todos os caminhos entre dois edificios,EdOr e EdDest.
Devolve uma lista com todos os caminhos possiveis*/
todos_caminhos_edificios(EdOr,EdDest,LTCamEd):-
    findall(LCam,caminho_edificios(EdOr,EdDest,LCam),LTCamEd).



/* ################################################################### */
/* ################### ALGORTIMOS AO NIVEL DO PISO ################### */
/* ################################################################### */

/* Algoritmo para encontrar todos os caminhos entre dois pisos,PisoOr e PisoDest.
Devolve uma lista de edificios percorridos e uma lista das ligaçoes (elevadores e|ou corredores)*/
caminho_pisos(PisoOr,PisoDest,LCam,LLig):- pisos(EdOr,LPisosOr),
    member(PisoOr,LPisosOr),
    pisos(EdDest,LPisosDest),
    member(PisoDest,LPisosDest), 
    caminho_edificios(EdOr,EdDest,LCam),
    segue_pisos(PisoOr,PisoDest,LCam,LLig).

segue_pisos(PisoDest,PisoDest,_,[]).

segue_pisos(PisoDest1,PisoDest,[EdDest],[elev(PisoDest1,PisoDest)]):- PisoDest\==PisoDest1,
    elevador(EdDest,LPisos), member(PisoDest1,LPisos), member(PisoDest,LPisos).

segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[cor(PisoAct,PisoSeg)|LOutrasLig]):- (corredor(EdAct,EdSeg,PisoAct,PisoSeg);corredor(EdSeg,EdAct,PisoSeg,PisoAct)),
segue_pisos(PisoSeg, PisoDest, [EdSeg|LOutrosEd], LOutrasLig).

segue_pisos(PisoAct,PisoDest,[EdAct,EdSeg|LOutrosEd],[elev(PisoAct,PisoAct1),cor(PisoAct1,PisoSeg)|LOutrasLig]):-
(corredor(EdAct,EdSeg,PisoAct1,PisoSeg);corredor(EdSeg,EdAct,PisoSeg,PisoAct1)),PisoAct1\==PisoAct,
elevador(EdAct,LPisos),member(PisoAct,LPisos),member(PisoAct1,LPisos),
segue_pisos(PisoSeg,PisoDest,[EdSeg|LOutrosEd],LOutrasLig).


/* Algortimo para encontrar o melhor caminho entre dois pisos com menor utilização de elevadores.
Em caso de empate, ou seja, mesmo numero de utilização de elevadores em dois caminhos, opta pelo que tem 
menos corredores no trajecto. */
melhor_caminho_pisos(PisoOr,PisoDest,LLigMelhor):-findall(LLig,caminho_pisos(PisoOr,PisoDest,_,LLig),LLLig),
menos_elevadores(LLLig,LLigMelhor,_,_).

menos_elevadores([LLig],LLig,NElev,NCor):-conta(LLig,NElev,NCor).

menos_elevadores([LLig|OutrosLLig],LLigR,NElevR,NCorR):-
    menos_elevadores(OutrosLLig,LLigM,NElev,NCor),
    conta(LLig,NElev1,NCor1),
    (((NElev1<NElev;(NElev1==NElev,NCor1<NCor)),!, NElevR is NElev1, NCorR is NCor1,LLigR=LLig);
    (NElevR is NElev,NCorR is NCor,LLigR=LLigM)).

conta([],0,0).
conta([elev(_,_)|L],NElev,NCor):-conta(L,NElevL,NCor),NElev is NElevL+1.
conta([cor(_,_)|L],NElev,NCor):-conta(L,NElev,NCorL),NCor is NCorL+1.


