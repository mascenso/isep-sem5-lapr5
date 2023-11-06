## Contents
- [Documentation Roadmap and Overview](#documentation-roadmap-and-overview)
	- [Purpose and Scope of the SAD](#purpose-and-scope-of-the-sad)
	- [How the SAD Is Organized](#how-the-sad-is-organized)
	- [How a View Is Documented](#how-a-view-is-documented)

# Documentation Roadmap and Overview

> Sub-parts of this section provide information that will help readers or users of the Software Architecture Document (SAD) quickly find information that will enable them to do their jobs. Readers of the SAD seeking an overview should begin here, as should readers interested in finding particular information to answer a specific question.

## Purpose and Scope of the SAD
> This section explains the SAD's overall purpose and scope, the criteria for deciding which design decisions are architectural (and therefore documented in the SAD), and which design decisions are non-architectural (and therefore documented elsewhere).

A arquitetura de software dum sistema é a estrutura ou estruturas desse sistema, que inclui elementos do software e suas proprieades visíveis externamente, e as relações entre ele (Bass 2012).

Este SAD descreve a arquitetura de software do sistema a desenvolver por solicitação da Autoridade Intermunicipal de Transportes (AIT) para gestão e planeamento de transportes públicos que permite a gestão bem como consulta pelo público em geral de diferentes redes de transportes, linhas e viagens, bem como o planeamento dos serviços de viaturas e tripulantes a efetuar nessas linhas.

Este SAD é desenvolvido num contexto académico de ensino-aprendizagem (no 5º semestre da LEI no ano letivo 2020-2021), em que várias competências estão a ser adquiridas ao longo do semestre pelos/as estudantes, ao mesmo tempo que desenvolvem o sistema.

Porque visa suportar o processo de ensino-aprendizagem, não tem como objetivo ser completo ou descrever a melhor arquitetura possível, mas servir de guia e exemplo, em linha com as competências a adquirir em cada iteração/sprint do projeto.

Embora os/as estudantes sejam destinatários principais do SAD, as competências a adquirir pelos/as estudantes nas várias UC do semestre permitem-lhe desempenhar diferentes papéis (diferentes partes interessadas/destinatários), e.g. eliciadores (de requisitos), analistas, arquitetos de software, programadores/"testers", administradores e operadores (ops) e utilizadores.

## How the SAD Is Organized
>This section provides a narrative description of the seven major sections of the SAD and the overall contents of each. Readers seeking specific information can use this section to help them locate it more quickly.
>This SAD is organized into the following seven sections:
> 1. This Documentation Roadmap and Overview provides information about this document and its intended audience. It provides the roadmap and document overview.
> 2. Architecture Background provides information about the software architecture. It describes the background and rationale for the software architecture. It explains the constraints and influences that led to the current architecture, and it describes the major architectural approaches that have been utilized in the architecture.
> 3. Views and
> 4. Mapping Between Views; both specify the software architecture.
> 5. Referenced Materials, provides look-up information for documents that are cited elsewhere in this SAD.
> 6. Glossary and Acronyms is an index of architectural elements and relations giving their definition, and where each is used in this SAD.

Este DAS/SAD adota a estrutura proposta acima.

## How a View Is Documented
>1. Primary Presentation
>> 1. Is usually graphical
>> 2. Should include a key that explains the notation
>> 3. Shows elements and relations among them
>> 4. Shows the information you want to convey about the view first
>> 5. Should identify elements that are external to scope of the view (if external entities are not clearly marked in the diagram, consider adding a context diagram)
> 2. Element Catalog
>	- Explains elements depicted in primary presentation and their properties
>	- Is usually a table with element name and textual description
>	- May contain interface documentation
>	- May contain behavior documentation
> 3. Variability Guide
>	- Points where system can be parameterized or reconfigured. Examples:
>		- Number of instances in a pool
>		- Support for plug-ins or add-ons
>		- Support for different versions of OS, database server or runtime environment
>	- Maybe the view is a reference architecture
>		- Provide guidelines to instantiate it
> 4. Other Information
>	- Description and rationale for important design decisions (including relevant rejected alternatives)
>	- Results of analysis, prototypes and experiments
>	- Context diagram
> 5. Parent View
>	- If the current view is the refinement of another view, indicate which one

Neste DAS/SAD será adotado a notação UML, nomeadamente (mas não exclusivamente) diagramas de componentes, de sequência, de pacotes e de nós. Tal, garante 1.1, 1.2 e 1.3.

A organização das vistas pela combinação do modelo C4 (diferentes níveis de abstração/granularidade) e modelo 4+1 vistas (vários pontos de vista da arquitetura) permite desde logo endereçar o requisito 1.4.

Pela adoção do modelo C4, o requisito 1.5 é endereçado.