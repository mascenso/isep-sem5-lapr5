/* Este ficheiro contém a base de conhecimento sobre as ligaçoes e entre edificios, pisos e elevadores, 
conforme a informação disponibilizada no caderno de encargos do ROBDRONEGO.
Para o efeito da prova de conceito, existem 4 edificios (a,b,c,d). 
Todos tem pelo menos um piso:
(a, [a1,a2]),
(b, [b1,b2,b3]),
(c, [c1,c2,c3,c4]),
(d, [d1,d2,d3]).
O edificio C e D estão a uma cota diferente do edificio A e B. C4 = B3 e D3 = C3.
Todos os pisos tem elevador:
a1,a2
b1,b2,b3
c1,c2,c3,c4
d1,d2,d3
Corredores nos pisos:
c2,d2
a2,b2
b2,c3
b2,d3
c3,d3
b3,c4
*/


%ligaçoes entre edificios
liga(a,b).
liga(b,c).
liga(b,d).
liga(c,d).


%ligaçoes entre pisos
pisos(a,[a1]).
pisos(b,[b1,b2,b3]).
pisos(c,[c1,c2,c3,c4]).
pisos(d,[d1,d2,d3]).


%elevadores em edificios e pisos servidos
elevador(a,[a1,a2]).
elevador(b,[b1,b2,b3]).
elevador(c,[c1,c2,c3,c4]).
elevador(d,[d1,d2,d3]).


%corredores entre pisos/edificios
corredor(c,d,c2,d2).
corredor(a,b,a2,b2).
corredor(b,c,b2,c3).
corredor(b,d,b2,d3).
corredor(c,d,c3,d3).
corredor(b,c,b3,c4).


%valores que para representar portas, corredores, elevadores, etc
circular(N):- member(N,[0]).

%valores da matriz para representar paredes
parede(N):- member(N,[1]).

%valores que para representar portas
porta(N):-member(N,[2]).

%valores que para representar elevadores
elev(N):-member(N,[3]).

%valores que para representar passagens
passagem(N):-member(N,[4]).


floor_map(a1, [
    [1, 1, 1, 1, 1],
    [1, 0, 0, 0, 3],
    [1, 0, 0, 0, 1],
    [1, 1, 2, 1, 1],
    [1, 1, 1, 1, 1]
]).

floor_map(a2, [
    [1, 1, 1, 1, 1],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 3],
    [1, 2, 1, 0, 1],
    [1, 1, 1, 4, 1]
]).


floor_map(b2, [
    [1, 1, 1, 4, 1],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 4],
    [3, 0, 1, 1, 1],
    [1, 1, 1, 1, 1]
]).

floor_map(c3, [
    [1, 1, 4, 3, 1],
    [1, 0, 0, 0, 1],
    [1, 0, 0, 0, 1],
    [1, 1, 2, 1, 1],
    [1, 1, 1, 1, 1]
]).

corredor_mtx(a2,b2,[[1, 1, 1],
                    [0, 0, 0],
                    [1, 1, 1]]).

corredor_mtx(b2, c3,[[1, 1],
                     [0, 0],
                     [1, 1]]).                    

%posiçao dos elevadores no map do respetivo floor (F1, cel(COL, LIN))
elev_pos(a1,cel(5,2)).
elev_pos(a2,cel(5,3)).
elev_pos(b2,cel(1,4)).
elev_pos(c3,cel(4,1)).

%posiçao das passagens no map do respetivo floor (E1, E2, cel(COL, LIN))
passag_pos(a2,b2,cel(5,3)).
passag_pos(b2,c3,cel(5,3)).

%posição incial no primeiro floor. Susbtituir depois pela posição inicial do robot aquando a integração com o JSON.
pos_init(a1,cel(3,3)).
pos_init(a2,cel(4,2)).
pos_init(b2,cel(3,2)).
pos_init(c3,cel(4,3)).
