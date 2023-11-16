/* Este ficheiro contém a base de conhecimento sobre as ligaçoes e entre edificios, pisos e elevadores.
Contém também conhecimento sobre o mapa de um dado piso ou seção deste para o percurso dos ROBOTS. */

%ligaçoes entre edificios 
liga(a,h).
liga(b,g).
liga(b,i).
liga(g,h).
liga(h,i).
liga(i,j).


%ligaçoes entre pisos
pisos(a,[a1]).
pisos(b,[b1,b2,b3,b4]).
pisos(g,[g2,g3,g4]).
pisos(h,[h1,h2,h3,h4]).
pisos(i,[i1,i2,i3,i4]).
pisos(j,[j1,j2,j3,j4]).


%elevadores em edificios e pisos servidos
elevador(b,[b1,b2,b3,b4]).
elevador(g,[g2,g3,g4]).
elevador(i,[i1,i2,i3,i4]).
elevador(j,[j1,j2,j3,j4]).


%corredores entre pisos/edificios
corredor(a,h,a1,h2).
corredor(b,g,b2,g2).
corredor(b,g,b3,g3).
corredor(b,i,b3,i3).
corredor(g,h,g2,h2).
corredor(g,h,g3,h3).
corredor(h,i,h2,i2).
corredor(i,j,i1,j1).
corredor(i,j,i2,j2).
corredor(i,j,i3,j3).


%valores da matriz para representar paredes
parede(N):- member(N, [1, 2, 3, 3.1, 2.1, 1.1]).

%valores que para representar portas, corredores, elevadores, etc
circular(N):- member(N, [0, 4, 5, 1.4, 1.3, 0.2, 0.3, 0.4, 0.5]).


/* MATRIZ EXEMPLO TP
0 - ROBOT poderá andar por lá
1 - obstaculo

L/C| 1 | 2 | 3 | 4 | 4 | 5 | 6 | 7 | 8 | --> EIXO X
 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
 2 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 |
 3 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 |
 4 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 |
 5 | 1 | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 1 |
 6 | 1 | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 1 |
 7 | 1 | 1 | 1 | 1 | 0 | 0 | 0 | 0 | 1 |
 |
EIXO Y

m(x,y, val).
m(col,lin,valor)

*/


% Base de conhecimento representando a matriz exemplo da TP
m(1,1,1).
m(2,1,1).
m(3,1,1).
m(4,1,1).
m(5,1,1).
m(6,1,1).
m(7,1,1).
m(8,1,1).

m(1,2,0).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,0).
m(6,2,0).
m(7,2,0).
m(8,2,1).

m(1,3,0).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,0).
m(6,3,0).
m(7,3,0).
m(8,3,1).

m(1,4,0).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,0).
m(6,4,0).
m(7,4,0).
m(8,4,1).

m(1,5,1).
m(2,5,1).
m(3,5,1).
m(4,5,1).
m(5,5,0).
m(6,5,0).
m(7,5,0).
m(8,5,1).

m(1,6,1).
m(2,6,1).
m(3,6,1).
m(4,6,1).
m(5,6,0).
m(6,6,0).
m(7,6,0).
m(8,6,1).

m(1,7,1).
m(2,7,1).
m(3,7,1).
m(4,7,1).
m(5,7,0).
m(6,7,0).
m(7,7,0).
m(8,7,1).


/*
MATRIZ MAZE - Edificio A, Piso 1
"map": [
        [3  ,  2,2.1,  2,  3,  2,2.1,  2,  3,  2,2.1,  2,  3,  2,  2,2.1,  3,  2,2.1,  2,  1,  0,  0],
        [1  ,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  4,  0],
        [1.1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  2,  2,  2,  0,  0,  1],
        [1  ,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  1],
        [1  ,  0,  0,  0,  3,  2,  2,0.2,  2,0.2,  2,  2,  2,0.2,  2,  2,  0,  0,  0,  0,  0,  0,  1],
        [1  ,  0,  0,  0,0.4,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  5],
        [3  ,  2,  2,  2,  2,  2,  2,0.3,1.3,  2,  2,  2,1.3,  2,  2,  2,  1,  0,  0,  0,  0,  0,  1],
        [1  ,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  1],
        [1  ,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  2,  2,  2,  2,  1],
        [1  ,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  1],
        [2  ,  2,  2,  2,  2,2.1,  2,  2,  2,  2,  2,2.1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  0]
    ],

Legenda:
0 - corredor;
4 - elevador;
1.4, 1.3, 0.2, 0.3, 0.4, 0.5 - portas;
1, 2, 3, 3.1, 2.1, 1.1 - paredes;
5- passagens entre edificios.
*/

/*
% Base de conhecimento representando a matriz do mapa
m(1,1,3).
m(2,1,2).
m(3,1,2.1).
m(4,1,2).
m(5,1,3).
m(6,1,2).
m(7,1,2.1).
m(8,1,2).
m(9,1,3).
m(10,1,2.1).
m(11,1,2).
m(12,1,3).
m(13,1,2).
m(14,1,2).
m(15,1,2.1).
m(16,1,3).
m(17,1,2).
m(18,1,2.1).
m(19,1,2).
m(20,1,1).
m(21,1,0).
m(22,1,0).

m(1,2,1).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,1).
m(6,2,0).
m(7,2,0).
m(8,2,0).
m(9,2,1).
m(10,2,0).
m(11,2,0).
m(12,2,0).
m(13,2,1).
m(14,2,0).
m(15,2,0).
m(16,2,0).
m(17,2,1).
m(18,2,0).
m(19,2,0).
m(20,2,1).
m(21,2,4).
m(22,2,0).

m(1,3,1.1).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,1).
m(6,3,0).
m(7,3,0).
m(8,3,0).
m(9,3,1).
m(10,3,0).
m(11,3,0).
m(12,3,0).
m(13,3,1).
m(14,3,0).
m(15,3,0).
m(16,3,0).
m(17,3,1).
m(18,3,2).
m(19,3,2).
m(20,3,2).
m(21,3,0).
m(22,3,1).

m(1,4,1).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,1).
m(6,4,0).
m(7,4,0).
m(8,4,0).
m(9,4,1).
m(10,4,0).
m(11,4,0).
m(12,4,0).
m(13,4,1).
m(14,4,0).
m(15,4,0).
m(16,4,0).
m(17,4,1).
m(18,4,0).
m(19,4,0).
m(20,4,0).
m(21,4,0).
m(22,4,1).

m(1,5,1).
m(2,5,0).
m(3,5,0).
m(4,5,0).
m(5,5,3).
m(6,5,2).
m(7,5,2).
m(8,5,0.2).
m(9,5,2).
m(10,5,0.2).
m(11,5,2).
m(12,5,2).
m(13,5,0.2).
m(14,5,2).
m(15,5,2).
m(16,5,0).
m(17,5,0).
m(18,5,0).
m(19,5,0).
m(20,5,0).
m(21,5,1).
m(22,5,0).

m(1,6,1).
m(2,6,0).
m(3,6,0).
m(4,6,0).
m(5,6,0.4).
m(6,6,0).
m(7,6,0).
m(8,6,0).
m(9,6,0).
m(10,6,0).
m(11,6,0).
m(12,6,0).
m(13,6,0).
m(14,6,0).
m(15,6,0).
m(16,6,0).
m(17,6,0).
m(18,6,0).
m(19,6,0).
m(20,6,0).
m(21,6,0).
m(22,6,5).

m(1,7,3).
m(2,7,2).
m(3,7,2).
m(4,7,2).
m(5,7,2).
m(6,7,2).
m(7,7,2).
m(8,7,0.3).
m(9,7,1.3).
m(10,7,2).
m(11,7,2).
m(12,7,2).
m(13,7,1.3).
m(14,7,2).
m(15,7,2).
m(16,7,2).
m(17,7,1).
m(18,7,0).
m(19,7,0).
m(20,7,1).
m(21,7,0).
m(22,7,1).

m(1,8,1).
m(2,8,0).
m(3,8,0).
m(4,8,0).
m(5,8,0).
m(6,8,0).
m(7,8,0).
m(8,8,0).
m(9,8,1).
m(10,8,0).
m(11,8,0).
m(12,8,0).
m(13,8,1).
m(14,8,0).
m(15,8,0).
m(16,8,0).
m(17,8,1).
m(18,8,0).
m(19,8,2).
m(20,8,2).
m(21,8,2).
m(22,8,1).

m(1,9,1).
m(2,9,0).
m(3,9,0).
m(4,9,0).
m(5,9,0).
m(6,9,0).
m(7,9,0).
m(8,9,0).
m(9,9,1).
m(10,9,0).
m(11,9,0).
m(12,9,0).
m(13,9,1).
m(14,9,0).
m(15,9,0).
m(16,9,0).
m(17,9,1).
m(18,9,0).
m(19,9,0).
m(20,9,0).
m(21,9,0).
m(22,9,1).

m(1,10,1).
m(2,10,0).
m(3,10,0).
m(4,10,0).
m(5,10,0).
m(6,10,0).
m(7,10,0).
m(8,10,0).
m(9,10,1).
m(10,10,0).
m(11,10,0).
m(12,10,0).
m(13,10,1).
m(14,10,0).
m(15,10,0).
m(16,10,0).
m(17,10,1).
m(18,10,0).
m(19,10,0).
m(20,10,0).
m(21,10,0).
m(22,10,1).

m(1,11,2).
m(2,11,2).
m(3,11,2).
m(4,11,2).
m(5,11,2.1).
m(6,11,2).
m(7,11,2).
m(8,11,2).
m(9,11,2).
m(10,11,2.1).
m(11,11,2).
m(12,11,2).
m(13,11,2.1).
m(14,11,2).
m(15,11,2).
m(16,11,2).
m(17,11,2).
m(18,11,2).
m(19,11,2).
m(20,11,2).
m(21,11,2).
m(22,11,0).

*/