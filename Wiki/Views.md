	## Contents
- [Views](#views)
	- [Introduction](#introduction)
	- [Nível 1](#nível-1)
		- [Vista Lógica](#vista-lógica)
		- [Vista de Processos](#vista-de-processos)
			- [SSD US1](#ssd-us1)
			- [SSD US2](#ssd-us2)
			- [(outros SSD arquiteturalmente relevantes)](#outros-ssd-arquiteturalmente-relevantes)
	- [Nível 2](#nível-2)
		- [Vista Lógica](#vista-lógica-1)
		- [Vista de Processos](#vista-de-processos-1)
			- [SSD US13 (Porquê esta US?)](#ssd-us13-porquê-esta-us)
			- [(outros SSD arquiteturalmente relevantes)](#outros-ssd-arquiteturalmente-relevantes-1)
		- [Vista de Implementação](#vista-de-implementação)
		- [Vista Física](#vista-física)
	- [Nível 3 (MDR)](#nível-3-mdr)
		- [Vista Lógica](#vista-lógica-2)
		- [Vista de Processos](#vista-de-processos-2)
			- [SD US01](#sd-us01)
			- [(outros SSD arquiteturalmente relevantes)](#outros-ssd-arquiteturalmente-relevantes-2)
		- [Vista de Implementação](#vista-de-implementação-1)
		- [Vista Física](#vista-física-1)
	- [Nível 3 (UI)](#nível-3-ui)
		- [Vista Lógica](#vista-lógica-3)
		- [Vista de Processos](#vista-de-processos-3)
		- [Vista de Implementação](#vista-de-implementação-2)
		- [Vista Física](#vista-física-2)
	- [Nível 3 (MDV)](#nível-3-mdv)
		- [Vista Lógica](#vista-lógica-4)
		- [Vista de Processos](#vista-de-processos-4)
		- [Vista de Implementação](#vista-de-implementação-3)
		- [Vista Física](#vista-física-3)
	- [Nível 3 (Planeamento)](#nível-3-planeamento)
		- [Vista Lógica](#vista-lógica-5)
		- [Vista de Processos](#vista-de-processos-5)
		- [Vista de Implementação](#vista-de-implementação-4)
		- [Vista Física](#vista-física-4)

# Views

## Introduction
Será adotada a combinação de dois modelos de representação arquitetural: C4 e 4+1.

O Modelo de Vistas 4+1 [[Krutchen-1995]](References.md#Kruchten-1995) propõe a descrição do sistema através de vistas complementares permitindo assim analisar separadamente os requisitos dos vários stakeholders do software, tais como utilizadores, administradores de sistemas, project managers, arquitetos e programadores. As vistas são deste modo definidas da seguinte forma:

- Vista lógica: relativa aos aspetos do software visando responder aos desafios do negócio;
- Vista de processos: relativa ao fluxo de processos ou interações no sistema;
- Vista de desenvolvimento: relativa à organização do software no seu ambiente de desenvolvimento;
- Vista física: relativa ao mapeamento dos vários componentes do software em hardware, i.e. onde é executado o software;
- Vista de cenários: relativa à associação de processos de negócio com atores capazes de os espoletar.

O Modelo C4 [[Brown-2020]](References.md#Brown-2020)[[C4-2020]](References.md#C4-2020) defende a descrição do software através de quatro níveis de abstração: sistema, contentor, componente e código. Cada nível adota uma granularidade mais fina que o nível que o antecede, dando assim acesso a mais detalhe de uma parte mais pequena do sistema. Estes níveis podem ser equiparáveis a mapas, e.g. a vista de sistema corresponde ao globo, a vista de contentor corresponde ao mapa de cada continente, a vista de componentes ao mapa de cada país e a vista de código ao mapa de estradas e bairros de cada cidade.
Diferentes níveis permitem contar histórias diferentes a audiências distintas.

Os níveis encontram-se definidos da seguinte forma:
- Nível 1: Descrição (enquadramento) do sistema como um todo;
- Nível 2: Descrição de contentores do sistema;
- Nível 3: Descrição de componentes dos contentores;
- Nível 4: Descrição do código ou partes mais pequenas dos componentes (e como tal, não será abordado neste DAS/SAD).

Pode-se dizer que estes dois modelos se expandem ao longo de eixos distintos, sendo que o Modelo C4 apresenta o sistema com diferentes níveis de detalhe e o Modelo de Vista 4+1 apresenta o sistema de diferentes perspetivas. Ao combinar os dois modelos torna-se possível representar o sistema de diversas perspetivas, cada uma com vários níveis de detalhe.

Para modelar/representar visualmente, tanto o que foi implementado como as ideias e alternativas consideradas, recorre-se à Unified Modeling Language (UML) [[UML-2020]](References.md#UML-2020) [[UMLDiagrams-2020]](References.md#UMLDiagrams-2020).

## Nível 1
### Vista Lógica

![N1-VL](diagramas/nivel1/N1-VL.png)

### Vista de Processos
#### SSD US1
![N1-VP-US1](diagramas/nivel1/N1-VP-US1.png)

#### SSD US2
![N1-VP-US2](diagramas/nivel1/N1-VP-US2.png)

#### (outros SSD arquiteturalmente relevantes)
[...]

#### US150
![US150](diagramas/nivel1/US150.svg)
___

#### US160
![US160](diagramas/nivel1/US160.svg)
___

#### US170
![US170](diagramas/nivel1/US170.svg)
___

#### US180
![US180](diagramas/nivel1/US180.svg)
___

#### US190
![US190](diagramas/nivel1/US190.svg)
___

#### US200
![US200](diagramas/nivel1/US200.svg)
___

#### US210
![US210](diagramas/nivel1/US210.svg)
___

#### US220
![US220](diagramas/nivel1/US220.svg)
___

#### US230
![US230](diagramas/nivel1/US230.svg)
___

#### US240
![US240](diagramas/nivel1/US240.svg)
___

#### US250
![US250](diagramas/nivel1/US250.svg)
___

#### US260
![US260](diagramas/nivel1/US260.svg)
___

#### US270
![US270](diagramas/nivel1/US270.svg)
___

#### US280
![US280](diagramas/nivel1/US280.svg)
___

#### US290
![US290](diagramas/nivel1/US290.svg)
___

#### US300
![US300](diagramas/nivel1/US300.svg)
___

#### US310
![US310](diagramas/nivel1/US310.svg)
___


#### US360
![US360](diagramas/nivel1/US360.svg)
___

#### US370
![US370](diagramas/nivel1/US370.svg)
___

#### US380
![US380](diagramas/nivel1/US380.svg)
___

#### US390
![US390](diagramas/nivel1/US390.svg)


## Nível 2
### Vista Lógica

![N2-VL](diagramas/nivel2/N2-VL.png)

### Vista de Processos

#### SSD US13 (Porquê esta US?)
#### US150
![US150](diagramas/nivel2/US150.svg)
___

#### US160
![US160](diagramas/nivel2/US160.svg)
___

#### US170
![US170](diagramas/nivel2/US170.svg)
___

#### US180
![US180](diagramas/nivel2/US180.svg)
___

#### US190
![US190](diagramas/nivel2/US190.svg)
___

#### US200
![US200](diagramas/nivel2/US200.svg)
___

#### US210
![US210](diagramas/nivel2/US210.svg)
___

#### US220
![US220](diagramas/nivel2/US220.svg)
___

#### US230
![US230](diagramas/nivel2/US230.svg)
___

#### US240
![US240](diagramas/nivel2/US240.svg)
___

#### US250
![US250](diagramas/nivel2/US250.svg)
___

#### US260
![US260](diagramas/nivel2/US260.svg)
___

#### US270
![US270](diagramas/nivel2/US270.svg)
___

#### US280
![US280](diagramas/nivel2/US280.svg)
___

#### US290
![US290](diagramas/nivel2/US290.svg)
___

#### US300
![US300](diagramas/nivel2/US300.svg)
___

#### US310
![US310](diagramas/nivel2/US310.svg)
___


#### US360
![US360](diagramas/nivel2/US360.svg)
___

#### US370
![US370](diagramas/nivel2/US370.svg)
___

#### US380
![US380](diagramas/nivel2/US380.svg)
___

#### US390
![US390](diagramas/nivel2/US390.svg)

#### (outros SSD arquiteturalmente relevantes)
[...]

### Vista de Implementação
![N2-VL](diagramas/nivel2/N2-VI.png)

### Vista Física

Uma proposta muito simplificada. 
![N2-VL](diagramas/nivel2/N2-VF.png)

De facto, deve-se ter em consideração os requisitos não funcionais ["Physical Contraints"](Background.md#Physical_Constraints).

## Nível 3 (MDR)
### Vista Lógica
Alternativa baseada numa arquitetura por camadas sobrepostas:
![N3-VL-MDR-alt1](diagramas/nivel3/MDR/N3-VL-MDR-alt1.png)

Alternativa baseada numa arquitetura por camadas concêntricas (Onion):
![N3-VL-MDR-alt2](diagramas/nivel3/MDR/N3-VL-MDR-alt2.png)

A alternativa Onion será a adotada.

### Vista de Processos

#### SD US01
#### US150
![US150](diagramas/nivel3/US150.svg)
___

#### US160
![US160](diagramas/nivel3/US160.svg)
___

#### US170
![US170](diagramas/nivel3/US170.svg)
___

#### US180
![US180](diagramas/nivel3/US180.svg)
___

#### US190
![US190](diagramas/nivel3/US190.svg)
___

#### US200
![US200](diagramas/nivel3/US200.svg)
___

#### US210
![US210](diagramas/nivel3/US210.svg)
___

#### US220
![US220](diagramas/nivel3/US220.svg)
___

#### US230
![US230](diagramas/nivel3/US230.svg)
___

#### US240
![US240](diagramas/nivel3/US240.svg)
___

#### US250
![US250](diagramas/nivel3/US250.svg)
___

#### US260
![US260](diagramas/nivel3/US260.svg)
___

#### US270
![US270](diagramas/nivel3/US270.svg)
___

#### US280
![US280](diagramas/nivel3/US280.svg)
___

#### US290
![US290](diagramas/nivel3/US290.svg)
___

#### US300
![US300](diagramas/nivel3/US300.svg)
___

#### US310
![US310](diagramas/nivel3/US310.svg)
___


#### US360
![US360](diagramas/nivel3/US360.svg)
___

#### US370
![US370](diagramas/nivel3/US370.svg)
___

#### US380
![US380](diagramas/nivel3/US380.svg)
___

#### US390
![US390](diagramas/nivel3/US390.svg)

#### (outros SSD arquiteturalmente relevantes)
[...]

### Vista de Implementação
![N3-VI-MDR-alt2](diagramas/nivel3/MDR/N3-VI-MDR-alt2.png)

Alguns detalhes mais (se existissem pais do que 4 níveis, podia ser considerado nível 4):

![N3.1-VI-MDR-alt2](diagramas/nivel3/MDR/N3.1-VI-MDR-alt2.png)

### Vista Física

Por agora, não existe necessidade de ser representada.

## Nível 3 (UI)
### Vista Lógica
TBD

### Vista de Processos
TBD

### Vista de Implementação
TBD

### Vista Física
TBD

## Nível 3 (MDV)
### Vista Lógica
TBD

### Vista de Processos
TBD

### Vista de Implementação
TBD

### Vista Física
TBD

## Nível 3 (Planeamento)
### Vista Lógica
TBD

### Vista de Processos
TBD

### Vista de Implementação
TBD

### Vista Física
TBD