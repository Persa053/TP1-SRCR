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
:- dynamic ano/1.
:- dynamic faseVacinacao/2.
:- dynamic data/3.

% Ano atual
ano(2021).

% Data atual
dataA(4,4,2021).

%solucoes(T,Q,S) :- findall(T,Q,S).

solucoes(X, XS, _) :- XS, assert(tmp(X)), fail.
solucoes(_, _, R) :- solucoesAux([], R).

solucoesAux(L, R) :- retract(tmp(X)), !, solucoesAux([X|L], R).
solucoesAux(R, R).



evolucao( Termo ) :-
  solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
      teste( Lista ).

insercao( Termo) :-
        assert( Termo ).
insercao( Termo) :-
        retract( Termo ),!,fail.



% fase1Vacincacao: #IdEnfermeiro,Nome,Idade,Género,#CentroSaude -> {V,F}
%1a fase 1 Vacinacao

%Lista dos cenas para a primeira fase de Vacinacao (Uma ou mais doença fdd; >65 anos; Medicxs; Enfermeirxs)

listaDoentesRiscoV(IDs) :- findall(ID, (utente(ID,_,_,_,_,_,_,_,Doencas), length(Doencas, R), R > 0), IDs).
listaVelhosV(IDs) :- findall(ID, (utente(ID,_,_,Idade,_,_,_,_,_), ano(Y), Y-Idade > 65), IDs).
listaMedicxV(IDs) :- findall(ID, utente(ID,_,_,_,_,_,_,medicx,_), IDs).
listaEnfermeirxV(IDs) :- findall(ID, utente(ID,_,_,_,_,_,_,enfermeirx,_), IDs).

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

% fase 2 Vacinacao
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
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a insercao de conhecimento
% Extensao predicado que permite a evolucao conhecimento: Termo - {V,F}











%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a removoção de conhecimento
% Extensão predicado que permite a involucao conhecimento: Termo - {V,F}


%involucao( Termo ) :-
%        solucoes( Invariante, -Termo::Invariante, Lista ),
%         remocao( Termo ),
%        teste( Lista ).

%remocao( Termo ):-
%        retract( Termo ).
%remocao( Termo ):-
%        assert( Termo ),!,fail.

%Permitir a definição de fases de vacinação, definindo critérios de inclusão de utentes nas diferentes fases (e.g., doenças crónicas, idade, profissão);

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacinadas uma vez;
peepsVac1Time(R) :- findall(ID,
                                  (vacinacao(_,ID,D,M,A, _, 1),
                                  utente(ID,_,_,_,_,_,_,_,_),
                                  dataA(Datual, Matual, Aatual),
                                  jaPassou(Datual, Matual, Aatual, D, M, A)),
                            X),
                    ordena(X, R).

% Identificar IDs de pessoas vacinadas duas vezes;
peepsVac2Time(R) :- findall(ID,
                                  (vacinacao(_,ID,D,M,A, _, 2),
                                  utente(ID,_,_,_,_,_,_,_,_),
                                  dataA(Datual, Matual, Aatual),
                                  jaPassou(Datual, Matual, Aatual, D, M, A)),
                            X),
                    ordena(X, R).

% Identificar IDs de pessoas não vacinadas;
peepsNoVac(R) :- findall(ID,
                              (utente(ID,_,_,_,_,_,_,_,_),
                              peepsVac1Time(V1),
                              peepsVac2Time(V2),
                              nao(pertence(ID,V1)),
                              nao(pertence(ID,V2))),
                         X),
                 ordena(X, R).

% Identificar IDs de pessoas que tem a primeira toma prevista;
peepsVac1Futura(R) :- findall(ID,
                                    (vacinacao(_,ID,D,M,A, _, 1),
                                     utente(ID,_,_,_,_,_,_,_,_),
                                     dataA(Datual, Matual, Aatual),
                                     nao(jaPassou(Datual, Matual, Aatual, D, M, A))),
                             X),
                     ordena(X,R).

% Identificar IDs de pessoas que tem a segunda toma prevista;
peepsVac2Futura(R) :- findall(ID,
                                    (vacinacao(_,ID,D,M,A, _, 2),
                                    utente(ID,_,_,_,_,_,_,_,_),
                                    dataA(Datual, Matual, Aatual),
                                    nao(jaPassou(Datual, Matual, Aatual, D, M, A))),
                              X),
                      ordena(X,R).

% Identificar IDs de pessoas não vacinadas e não tem toma prevista;
peepsNoVacFutura(R) :- findall(ID,
                              (utente(ID,_,_,_,_,_,_,_,1),
                              peepsNoVac(Vn),
                              peepsVac1Futura(V1),
                              peepsVac2Futura(V2),
                              nao(pertence(ID,V1)),
                              nao(pertence(ID,V2)),
                              pertence(ID,Vn)),
                         X),
                 ordena(X, R).
%


% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% --------------------------- Auxiliares ------------------------------
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------

% Data é anterior à atual
%----------nao sei se é preciso o "!, true." mas funciona das duas maneiras---------
jaPassou(_, _, Aatual, _, _, A) :- A < Aatual, !, true.
jaPassou(_, Matual, Aatual, _, M, A) :- A =:= Aatual, M < Matual, !, true.
jaPassou(Datual, Matual, Aatual, D, M, A) :-  A =:= Aatual, M =:= Matual, D < Datual, !, true.

% Ve se um elem pertence à lista;
pertence( X,[X|_] ).
pertence( X,[Y|L] ) :- X \= Y, pertence( X,L ).

% Adiciona um elem à lista;
add(E, L, [E|L]).

% Concatena uma lista
concat([], L, L).
concat([H|T], L, R) :- add(H, N, R), concat(T, L, N).

% Insere um elemnto numa lista ordenada, mantendo a lista ordenada;
inserec(X,[],[X]).
inserec(N,[H|T],[N,H|T]):- N=<H.
inserec(X,[H|T],[H|L]):- X > H, inserec(X,T,L).

% Ordena uma lista;
ordena([X],[X]).
ordena([H|T],F):- ordena(T,N), inserec(H,N,F).

% Remove elems repetidos de uma lista;
repRemove([],[]).
repRemove([X|A],R) :- elemRemove(X,A,L),
                      repRemove(L,T),
                      R = [X|T].
% Remove a primeira ocurrencia de um elem numa lista;
elemRemove(_,[],[]).
elemRemove(A,[A|Y],T) :- elemRemove(A,Y,T).
elemRemove(A,[X|Y],T) :- X \== A,
                          elemRemove(A,Y,R),
                      			T = [X|R].
% Faz o não cenas;
nao( Questao ) :- Questao, !, fail.
nao( _ ).
