%---------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3ºAno

%---------------------------------------------------------------------
% TRABALHO PRÁTICO: PARTE 1    2020/2021

%---------------------------------------------------------------------
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag( answer_write_options,[max_depth(0)] ).

:- op( 900,xfy,'::' ).
:- dynamic faseVacinacao/2.
:- dynamic data/3.
:- use_module(conhecimento).

% Data atual
dataA(4,4,2021).

solucoes(X, XS, _) :- XS, assert(tmp(X)), fail.
solucoes(_, _, R) :- solucoesAux([], R).

solucoesAux(L, R) :- retract(tmp(X)), !, solucoesAux([X|L], R).
solucoesAux(R, R).

%#AInesÉFixe%%%%%evolucao( Termo ) :-
%#AInesÉFixe%%%%%  solucoes( Invariante,+Termo::Invariante,Lista ),--------------------------------------------------------------------------------------------------------------------------
%#AInesÉFixe%%%%%    insercao( Termo ),
%#AInesÉFixe%%%%%      teste( Lista ).

% fase1Vacincacao: #IdEnfermeiro,Nome,Idade,Género,#CentroSaude -> {V,F}
%1a fase 1 Vacinacao

%Lista dos cenas para a primeira fase de Vacinacao (Uma ou mais doença fdd; >65 anos; Medicxs; Enfermeirxs)
% Identifica utentes com uma ou mais Doenças; listaDoentesRiscoV(lista de IDs de utentes). -> {V,F}
listaDoentesRiscoV(IDs) :- findall(ID, (utente(ID,_,_,_,_,_,_,_,Doencas), length(Doencas, R), R > 0), IDs).

%---------------------------------------------------------------------
% Identifica utentes com mais de 65 anos de idade; listaVelhosV(lista de IDs de utentes). -> {V,F}
listaVelhosV(IDs) :- findall(ID, (utente(ID,_,_,Idade,_,_,_,_,_), dataA(_,_,A), A-Idade > 65), IDs).

%---------------------------------------------------------------------
% Identifica utentes que são médicos de profissão; listaMedicxV(lista de IDs de utentes). -> {V,F}
listaMedicxV(IDs) :- findall(ID, utente(ID,_,_,_,_,_,_,medicx,_), IDs).

%---------------------------------------------------------------------
% Identifica utentes que são enfermeiros de profissão; listaEnfermeirxV(lista de IDs de utentes). -> {V,F}
listaEnfermeirxV(IDs) :- findall(ID, utente(ID,_,_,_,_,_,_,enfermeirx,_), IDs).

%---------------------------------------------------------------------
% Identifica todos os eleitos para a primeira fase de vacinaçao; listafaseVacinacao1(lista de IDs de utentes). -> {V,F}
listafaseVacinacao1(IDs) :- listaEnfermeirxV(E),
                            listaMedicxV(M),
                            listaVelhosV(V),
                            listaDoentesRiscoV(D),
                            concat([], D, Z),
                            concat(V, Z, W),
                            concat(M, W, Y),
                            concat(E, Y, X),
                            repRemove(X, Xs),
                            ordena(Xs, IDs).

%---------------------------------------------------------------------
% Identifica todos os eleitos para a segunda fase de vacinaçao; listafaseVacinacao2(lista de IDs de utentes). -> {V,F}
listafaseVacinacao2(IDs) :- findall(ID, (listafaseVacinacao1(Xs), utente(ID,_,_,_,_,_,_,_,_), nao(pertence(ID, Xs))), IDs).

%---------------------------------------------------------------------
% Identificar utentes/centrosaude/staff por critérios de seleção



%utente(20, 763487435, 'Luis Veloso', 07-08-1978, 'luisveloso@gmail.com', 927465839, aveiro, marinheiro, []).
%Utente: #Idutente, Nº Segurança_Social, Nome, Data_Nasc, Email, Telefone, Morada, Profissão, [Doenças_Crónicas], #CentroSaúde ↝ { 𝕍, 𝔽}


utenteId(Id, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

utenteNrSs(Ss, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

utenteNome(N, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

utenteDataNascimento(_, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

utenteEmail(E, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

utenteTelefone(Tlf, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

utenteMorada(M, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

utenteProfissao(P, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC), R).

%--------- CentroSaude


%centro_saúde: #Idcentro, Nome, Morada, Telefone, Email ↝ { 𝕍, 𝔽}
%centrosaude(1, 'Centro de Saúde de Braga', 'Largo Paulo Orósio, 4700-031 Braga', 253928647, 'csbraga@gmail.com').

centroSaudeId(Id, R) :-
        solucoes(centrosaude(Id,N,M,Tlf,E), centrosaude(Id,N,M,Tlf,E), R).

centroSaudeNome(N, R) :-
        solucoes(centrosaude(Id,N,M,Tlf,E), centrosaude(Id,N,M,Tlf,E), R).

centroSaudeMorada(M, R) :-
        solucoes(centrosaude(Id,N,M,Tlf,E), centrosaude(Id,N,M,Tlf,E), R).

centroSaudeTelefone(Tlf, R) :-
        solucoes(centrosaude(Id,N,M,Tlf,E), centrosaude(Id,N,M,Tlf,E), R).

centroSaudeEmail(E, R) :-
        solucoes(centrosaude(Id,N,M,Tlf,E), centrosaude(Id,N,M,Tlf,E), R).


%--------- Staff

%staff: #Cstaff, #Idcentro, Nome, email ↝ { 𝕍, 𝔽 }
%staff(1, 1, 'Monica Sintra','monicas@gmail.com').

staffId(Id, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).

staffCentroSaude(Cs, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).

staffNome(N, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).

staffEmail(E, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).



%--------- MedicoFamilia

% medicoFamilia: #IdMedico,Nome,Idade,Género,#CentroSaude -> {V,F}
%medico(1,'Ester Domingues',47,'F',1).


medicoId(Id, R) :-
        solucoes(medico(Id,N,I,G,Cs), medico(Id,N,I,G,Cs), R).

medicoNome(N, R) :-
        solucoes(medico(Id,N,I,G,Cs), medico(Id,N,I,G,Cs), R).

medicoIdade(I, R) :-
        solucoes(medico(Id,N,I,G,Cs), medico(Id,N,I,G,Cs), R).

medicoGenero(G, R) :-
        solucoes(medico(Id,N,I,G,Cs), medico(Id,N,I,G,Cs), R).

medicoCentroSaude(Cs, R) :-
        solucoes(medico(Id,N,I,G,Cs), medico(Id,N,I,G,Cs), R).


%--------- Enfermeiro

% enfermeiro: #IdEnfermeiro,Nome,Idade,Género,#CentroSaude -> {V,F}
% enfermeiro(1,'Márcia Araujo',27,'F',1).

enfermeiroID(Id, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroNome(N, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroIdade(I, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroGenero(G, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroCservico(Cs, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a insercao de conhecimento
% Extensao predicado que permite a evolucao conhecimento: Termo - {V,F}

evolucao( Termo ) :- findall(Invariante, +Termo::Invariante, Lista),
                     insercao( Termo ),
                     teste( Lista ).

insercao( Termo ) :- assert( Termo ).
insercao( Termo ) :- retract( Termo ), !, fail.

teste( [] ).
teste( [R|LR] ) :- R, teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a removoção de conhecimento
% Extensão predicado que permite a involucao conhecimento: Termo - {V,F}


involucao( Termo ) :- solucoes( Invariante, -Termo::Invariante, Lista ),
                      remocao( Termo ),
                      teste( Lista ).

remocao( Termo ) :- retract( Termo ).
remocao( Termo ) :- assert( Termo ),!,fail.

%Permitir a definição de fases de vacinação, definindo critérios de inclusão de utentes nas diferentes fases (e.g., doenças crónicas, idade, profissão);

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacinadas com a 1a dose; peepsVac1Time(Lista de IDs de utentes). -> {V,F}
peepsVac1Time(R) :- findall(ID,
                                  (vacinacao(_,ID,D,M,A, _, 1),
                                  utente(ID,_,_,_,_,_,_,_,_),
                                  jaPassou(D, M, A)),
                            X),
                    ordena(X, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacinadas com 2a dose; peepsVac2Time(Lista de IDs de utentes). -> {V,F}
peepsVac2Time(R) :- findall(ID,
                                  (vacinacao(_,ID,D,M,A, _, 2),
                                  utente(ID,_,_,_,_,_,_,_,_),
                                  jaPassou(D, M, A)),
                            X),
                    ordena(X, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacinadas pelo menos uma vez; peepsVac(Lista de IDs de utentes). -> {V,F}
peepsVac(R) :- peepsVac1Time(V1),
               peepsVac2Time(V2),
               concat(V1, V2, X),
               repRemove(X, Y),
               ordena(Y, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacinadas com as duas doses; fullVac(Lista de IDs de utentes). -> {V,F}
fullVac(R) :- findall(ID,
                          (utente(ID,_,_,_,_,_,_,_,_),
                          peepsVac1Time(V1),
                          pertence(ID,V1),
                          peepsVac2Time(V2),
                          pertence(ID,V2)),
                      X),
              ordena(X, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas não vacinadas; peepsNoVac(Lista de IDs de utentes). -> {V,F}
peepsNoVac(R) :- findall(ID,
                              (utente(ID,_,_,_,_,_,_,_,_),
                              peepsVac(V),
                              nao(pertence(ID, V))),
                         X),
                 ordena(X, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas que tem a primeira toma prevista; peepsVac1Futura(lista de IDs de utentes). -> {V,F}
peepsVac1Futura(R) :- findall(ID,
                                    (vacinacao(_,ID,D,M,A, _, 1),
                                     utente(ID,_,_,_,_,_,_,_,_),
                                     nao(jaPassou(D, M, A))),
                             X),
                     ordena(X,R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas que tem a segunda toma prevista; peepsVac2Futura(lista de IDs de utentes). -> {V,F}
peepsVac2Futura(R) :- findall(ID,
                                    (vacinacao(_,ID,D,M,A, _, 2),
                                    utente(ID,_,_,_,_,_,_,_,_),
                                    nao(jaPassou(D, M, A))),
                              X),
                      ordena(X,R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas não vacinadas e não tem toma prevista; peepsNoVacFutura(lista de IDs de utentes). -> {V,F}
peepsNoVacFutura(R) :- findall(ID,
                                (utente(ID,_,_,_,_,_,_,_,_),
                                peepsNoVac(X),
                                pertence(ID,X),
                                peepsVac1Futura(Y),
                                nao(pertence(ID,Y)),
                                peepsVac2Futura(Z),
                                nao(pertence(ID,Z))),
                         A),
                 ordena(A, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacianadas indevidamente; indevidamente(lista de IDs de utentes). -> {V,F}
indevidamente(R) :- findall(ID,
                                      (vacinacao(_, ID, D, M, A,_,_),
                                      utente(ID,_,_,_,_,_,_,_,_),
                                      jaPassou(D, M, A),
                                      listafaseVacinacao1(F1),
                                      nao(pertence(ID, F1)),
                                      nao(verifica2toma(F1))),
                                   Y),
                           repRemove(Y, X),
                           ordena(X, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas não vacinadas nenhuma vez e candidatas à primeira fase; candidatosFase1(lista de IDs de utentes). -> {V,F}
candidatosFase1(R) :- findall(ID,
                                      (utente(ID,_,_,_,_,_,_,_,_),
                                      listafaseVacinacao1(F1),
                                      pertence(ID, F1),
                                      peepsVac(V),
                                      nao(pertence(ID, V))),
                              X),
                      ordena(X, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas que falta a segunda toma da vacina; falta2toma(lista de IDs de utentes). -> {V,F}
falta2toma(R) :- findall(ID,
                                  (utente(ID,_,_,_,_,_,_,_,_),
                                  peepsVac1Time(V1),
                                  pertence(ID, V1),
                                  peepsVac2Time(V2),
                                  nao(pertence(ID, V2))),
                              X),
                      ordena(X, R).

% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% --------------------------- Auxiliares ------------------------------
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------

%---------------------------------------------------------------------
% Verifica se uma data é anterior à atual; jaPassou(Dia, Mes, Ano). -> {V,F}
%----------nao sei se é preciso o "!, true." mas funciona das duas maneiras-------------------------------------------------------------------------------------------------------------------
jaPassou(_, _, A) :- dataA(_,_, Aatual),
                     A < Aatual, !, true.
jaPassou( _, M, A) :- dataA(_, Matual, Aatual),
                      A =:= Aatual,
                      M < Matual, !, true.
jaPassou(D, M, A) :- dataA(Datual, Matual, Aatual),
                     A =:= Aatual,
                     M =:= Matual,
                     D < Datual, !, true.

%---------------------------------------------------------------------
% Verifica se um utente ja tomou as duas doses da vacina; veraux(ID do utente). -> {V,F}
veraux(ID) :- utente(ID,_,_,_,_,_,_,_,_),
              vacinacao(_,ID,D1,M1,A1,_,1),
              jaPassou(D1, M1, A1),
              vacinacao(_,ID,D2,M2,A2,_,2),
              jaPassou(D2, M2, A2).

%---------------------------------------------------------------------
% Verifica se todos os utentes de uma lista ja tomaram a 2a toma da vacina; verifica2toma(Lista de IDs de utentes). -> {V,F}
verifica2toma([]).
verifica2toma([H|T]) :- veraux(H), verifica2toma(T).

%---------------------------------------------------------------------
% Ve se um elem pertence à lista; pertence(elemento, lista de elementos). -> {V,F}
pertence( X,[X|_] ).
pertence( X,[Y|L] ) :- X \= Y, pertence( X,L ).

%---------------------------------------------------------------------
% Adiciona um elem à cabeça da lista; add(elemento, lista de elementos, lista de elementos). -> {V,F}
add(E, L, [E|L]).

%---------------------------------------------------------------------
% Concatena duas listas; concat(lista de elementos, lista de elementos, lista de elementos). -> {V,F}
concat([], L, L).
concat([H|T], L, R) :- add(H, N, R), concat(T, L, N).

%---------------------------------------------------------------------
% Insere um elemnto numa lista ordenada, mantendo a lista ordenada de forma crescente; inserec(elemento, lista de elementos, lista de elementos). -> {V,F}
inserec(X,[],[X]).
inserec(N,[H|T],[N,H|T]):- N=<H.
inserec(X,[H|T],[H|L]):- X > H, inserec(X,T,L).

%---------------------------------------------------------------------
% Ordena uma lista; ordena(lista de elementos, lista de elementos). -> {V,F}
ordena([X],[X]).
ordena([H|T],F):- ordena(T,N), inserec(H,N,F).

%---------------------------------------------------------------------
% Remove elems repetidos de uma lista; repRemove(lista de elementos, lista de elementos). -> {V,F}
repRemove([],[]).
repRemove([X|A],R) :- elemRemove(X,A,L),
                      repRemove(L,T),
                      R = [X|T].

%---------------------------------------------------------------------
% Remove a primeira ocurrencia de um elem numa lista; elemRemove(elemento, lista de elementos, lista de elementos). -> {V,F}
elemRemove(_,[],[]).
elemRemove(A,[A|Y],T) :- elemRemove(A,Y,T).
elemRemove(A,[X|Y],T) :- X \== A,
                         elemRemove(A,Y,R),
                      	 T = [X|R].

%---------------------------------------------------------------------
% Faz o não cenas;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nao( Questao ) :- Questao, !, fail.
nao( _ ).