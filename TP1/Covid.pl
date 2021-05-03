%---------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3ºAno

%---------------------------------------------------------------------
% TRABALHO PRÁTICO: PARTE 2    2020/2021

%---------------------------------------------------------------------
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag( answer_write_options,[max_depth(0)] ).

:- op(900, xfy, '::').
:- op(996, xfy, '&&' ).
:- op(997, xfy, '$$' ).
:- op(998, xfx, '=>' ).
:- op(999, xfx, '<=>' ).
:- dynamic faseVacinacao/2.
:- dynamic excecao/1.
:- dynamic nulo/1.
:- dynamic incerto/2.
:- dynamic interdito/2.
:- dynamic impreciso/2.
:- dynamic incertoVacinacao/4.
:- dynamic interditoVacinacao/4.
:- dynamic imprecisoVacinacao/4.
:- use_module(conhecimento).

% Data atual consoante o momento da utilização
dataA(Dia, Mes, Ano) :- get_time(TS),
                            stamp_date_time(TS,DateTime,'local'),
                            arg(3,DateTime,Dia),
                            arg(2,DateTime,Mes),
                            arg(1,DateTime,Ano).

% predicado que calcula todas as soluções de uma dada variável numa lista
solucoes(X, XS, _) :- XS, assert(tmp(X)), fail.
solucoes(_, _, R) :- solucoesAux([], R).

solucoesAux(L, R) :- retract(tmp(X)), !, solucoesAux([X|L], R).
solucoesAux(R, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lista dos cenas para a primeira fase de Vacinacao (Uma ou mais doenças crónicas; >65 anos; Medicxs; Enfermeirxs)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Identifica utentes com uma ou mais Doenças; listaDoentesRiscoV(lista de IDs de utentes). -> {V,F}
listaDoentesRiscoV(IDs) :- findall(ID, (utente(ID,_,_,_,_,_,_,_,Doencas,_), length(Doencas, R), R > 0), IDs).

%---------------------------------------------------------------------
% Identifica utentes com mais de 65 anos de idade; listaVelhosV(lista de IDs de utentes). -> {V,F}
listaVelhosV(IDs) :- findall(ID, (utente(ID,_,_,Idade,_,_,_,_,_,_), dataA(_,_,A), A-Idade > 65), IDs).

%---------------------------------------------------------------------
% Identifica utentes que são médicos de profissão; listaMedicxV(lista de IDs de utentes). -> {V,F}
listaMedicxV(IDs) :- findall(ID, utente(ID,_,_,_,_,_,_,medicx,_,_), IDs).

%---------------------------------------------------------------------
% Identifica utentes que são enfermeiros de profissão; listaEnfermeirxV(lista de IDs de utentes). -> {V,F}
listaEnfermeirxV(IDs) :- findall(ID, utente(ID,_,_,_,_,_,_,enfermeirx,_,_), IDs).

%---------------------------------------------------------------------
% Identifica o staff; listaStaff(lista de IDs de Staff). -> {V,F}
listaStaff(IDs) :- findall(ID, staff(ID,_,_,_), IDs).

%---------------------------------------------------------------------
% Identifica os centros de saúde; listaCentroSaude(lista de IDs de centros de saúde). -> {V,F}
listaCentroSaude(IDs) :- findall(ID, centrosaude(ID,_,_,_,_), IDs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Identificar fases de vacinação
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
listafaseVacinacao2(IDs) :- findall(ID,(listafaseVacinacao1(Xs), utente(ID,_,_,_,_,_,_,_,_,_), nao(pertence(ID, Xs))), IDs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Identificar utentes/centrosaude/staff por critérios de seleção
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%utente(20, 763487435, 'Luis Veloso', 07-08-1978, 'luisveloso@gmail.com', 927465839, aveiro, marinheiro, [], 7).
%Utente: #Idutente, Nº Segurança_Social, Nome, Data_Nasc, Email, Telefone, Morada, Profissão, [Doenças_Crónicas], #CentroSaúde -> {V,F}


utenteId(Id, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

utenteNrSs(Ss, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

utenteNome(N, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

utenteDataNascimento(_, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

utenteEmail(E, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

utenteTelefone(Tlf, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

utenteMorada(M, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

utenteProfissao(P, R) :-
        solucoes(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,Cs), R).

%--------- CentroSaude


%centro_saúde: #Idcentro, Nome, Morada, Telefone, Email -> {V,F}


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

%staff: #Cstaff, #Idcentro, Nome, email -> {V, F}


staffId(Id, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).

staffCentroSaude(Cs, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).

staffNome(N, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).

staffEmail(E, R) :-
        solucoes(staff(Id,Cs,N,E), staff(Id,Cs,N,E), R).



%--------- Medico

% medico: #IdMedico,Nome,Idade,Género,#CentroSaude -> {V,F}



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


enfermeiroID(Id, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroNome(N, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroIdade(I, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroGenero(G, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).

enfermeiroCservico(Cs, R) :- solucoes(enfermeiro(Id,N,I,G,Cs), enfermeiro(Id,N,I,G,Cs), R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a insercao e remocao de conhecimento
% Extensao predicado que permite a evolucao conhecimento: Termo - {V,F}

evolucao( Termo ) :- findall(Invariante, +Termo::Invariante, Lista),
                     insercao( Termo ),
                     teste( Lista ).

insercao( Termo ) :- assert( Termo ).
insercao( Termo ) :- retract( Termo ), !, fail.

teste( [] ).
teste( [R|LR] ) :- R, teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicados para registar utentes, centros de saúde, staff e vacinações
registar_utente(A,B,C,D,E,F,G,H,I,J) :- evolucao(utente(A,B,C,D,E,F,G,H,I,J)).
registar_centroSaude(A,B,C,D,E) :- evolucao(centrosaude(A,B,C,D,E)).
registar_staff(A,B,C,D) :- evolucao(staff(A,B,C,D)).
registar_vacinacao(A,B,C,D,E,F,G) :- evolucao(vacinacao(A,B,C,D,E,F,G)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os utentes tem ID's distintos;
+utente(Id,_,_,_,_,_,_,_,_,_) :: (findall( Id, utente(Id,_,_,_,_,_,_,_,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os utentes tem números da SS distintos;
+utente(_,Ss,_,_,_,_,_,_,_,_) :: (findall( Ss, utente(_,Ss,_,_,_,_,_,_,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os utentes tem emails distintos;
+utente(_,_,_,_,E,_,_,_,_,_) :: (findall( E, utente(_,_,_,_,E,_,_,_,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os utentes tem números de telefone distintos;
+utente(_,_,_,_,_,T,_,_,_,_) :: (findall( T, utente(_,_,_,_,_,T,_,_,_,_), R),
                                    length(R, X), X==1).

% Todos os utentes estao destacados num centro de saude existente;
+utente(_,_,_,_,_,_,_,_,_,Cs) :: (findall( Cs, centrosaude(Cs,_,_,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os centros de saude tem ID's distintos;
+centrosaude(Id,_,_,_,_) :: (findall( Id, centrosaude(Id,_,_,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os centros de saude tem moradas distintas;
+centrosaude(_,_,M,_,_) :: (findall( M, centrosaude(_,_,M,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os centros de saude tem numeros de telefone distintos;
+centrosaude(_,_,_,T,_) :: (findall( T, centrosaude(_,_,_,T,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os centros de saude tem emails distintos;
+centrosaude(_,_,_,_,E) :: (findall( E, centrosaude(_,_,_,_,E), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os membros do staff tem ID's distintos;
+staff(Id,_,_,_) :: (findall( Id, staff(Id,_,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os membros do staff tem de estar destacados num centro de saude existente distintos;
+staff(_,Cs,_,_) :: (findall( Cs, centrosaude(Cs,_,_,_,_), R),
                                    length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos os membros do staff tem emails distintos;
+staff(_,_,_,M) :: (findall( M, staff(_,_,_,M), R),
                                      length(R, X), X==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Todos as vacinas tem um mebro do staff existente;
+vacinacao(S,_,_,_,_,_,_) :: (findall( S, staff(S,_,_,_), R),
                                      length(R, X), X==1).

+vacinacao(_,U,_,_,_,_,_) :: (findall( U, utente(U,_,_,_,_,_,_,_,_,_), R),
                                      length(R, X), X==1).

% Todas as vacinas tem um utente por toma, por vacina;
+vacinacao(_,U,_,_,_,_,Toma) :: (findall((U, Toma), vacinacao(_,U,_,_,_,_,Toma), R),
                                      length(R, X), X==1).


+vacinacao(_,U,_,_,_,Tipo,Toma) :: (Toma == 1;(findall((U, Tipo), vacinacao(_,U,_,_,_,Tipo,_), R),
                                      length(R, X), X==2)).

% Staff e Utente mesmo centro de saude.
+vacinacao(S,U,_,_,_,_,_) :: (findall((U, S, Cs), (utente(U,_,_,_,_,_,_,_,_,Cs), staff(S,Cs,_,_)), R),
                                      length(R, X), X==1).
                                      
% Segunda toma ser depois da primeira.
+vacinacao(_,Utente,Dia2,Mes2,Ano2,_,Toma) :: 
                    (Toma == 1;(vacinacao(_,Utente,Dia1,Mes1,Ano1,_,1),
                     Toma == 2,comparaDatas(Dia1,Mes1,Ano1,Dia2,Mes2,Ano2))).
                                
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a removoção de conhecimento
% Extensão predicado que permite a involucao conhecimento: Termo -> {V,F}


involucao( Termo ) :- solucoes( Invariante, -Termo::Invariante, Lista ),
                      remocao( Termo ),
                      teste( Lista ).

remocao( Termo ) :- retract( Termo ).
remocao( Termo ) :- assert( Termo ),!,fail.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Predicados para remover utentes, centros de saúde, staff e vacinações
remover_utente(A,B,C,D,E,F,G,H,I,J) :- involucao(utente(A,B,C,D,E,F,G,H,I,J)).
remover_centroSaude(A,B,C,D,E) :- involucao(centrosaude(A,B,C,D,E)).
remover_staff(A,B,C,D) :- involucao(staff(A,B,C,D)).
remover_vacinacao(A,B,C,D,E,F,G) :- involucao(vacinacao(A,B,C,D,E,F,G)).

% remocao de conhecimento incerto
removeIncerto(IdUt,utente) :-
        incerto(IdUt,utente),
        remocao(incerto(IdUt,utente)).
removeIncerto(IdUt,utente).

removeIncerto(A,B,C,D,E,vacinacao) :-
        incertoVacinacao(A,B,C,D,E,vacinacao),
        remocao(incertoVacinacao(A,B,C,D,E,vacinacao)).
removeIncerto(A,B,C,D,E,vacinacao).

removeIncerto(Id,staff) :-
        incerto(Id,staff),
        remocao(incerto(Id,staff)).
removeIncerto(Id,staff).

removeIncerto(Id,centrosaude) :-
        incerto(Id,centrosaude),
        remocao(incerto(Id,centrosaude)).
removeIncerto(Id,centrosaude).

removeIncerto(Id,medico) :-
        incerto(Id,medico),
        remocao(incerto(Id,medico)).
removeIncerto(Id,medico).

removeIncerto(Id,enfermeiro) :-
        incerto(Id,enfermeiro),
        remocao(incerto(Id,enfermeiro)).
removeIncerto(Id,enfermeiro).

% remocao de conhecimento interdito
removeInterdito(Id,utente) :-
        interdito(Id,utente),
        remocao(interdito(Id,utente)).
removeInterdito(Id,utente).

removeInterdito(A,B,C,D,E,vacinacao) :-
        interditoVacinacao(A,B,C,D,E,vacinacao),
        remocao(interditoVacinacao(A,B,C,D,E,vacinacao)).
removeInterdito(A,B,C,D,E,vacinacao).

removeInterdito(Id,staff) :-
        interdito(Id,staff),
        remocao(interdito(Id,staff)).
removeInterdito(Id,staff).

removeInterdito(Id,centrosaude) :-
        interdito(Id,centrosaude),
        remocao(interdito(Id,centrosaude)).
removeInterdito(Id,centrosaude).

removeInterdito(Id,medico) :-
        interdito(Id,medico),
        remocao(interdito(Id,medico)).
removeInterdito(Id,medico).

removeInterdito(Id,enfermeiro) :-
        interdito(Id,enfermeiro),
        remocao(interdito(Id,enfermeiro)).
removeInterdito(Id,enfermeiro).

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacinadas com a 1a dose; peepsVac1Time(Lista de IDs de utentes). -> {V,F}
peepsVac1Time(R) :- findall(ID,
                                  (vacinacao(_,ID,D,M,A, _, 1),
                                  utente(ID,_,_,_,_,_,_,_,_,_),
                                  jaPassou(D, M, A)),
                            X),
                    repRemove(X,B),
                    ordena(B, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacinadas com 2a dose; peepsVac2Time(Lista de IDs de utentes). -> {V,F}
peepsVac2Time(R) :- findall(ID,
                                  (vacinacao(_,ID,D,M,A, _, 2),
                                  utente(ID,_,_,_,_,_,_,_,_,_),
                                  jaPassou(D, M, A)),
                            X),
                    repRemove(X,B),
                    ordena(B, R).

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
                          (utente(ID,_,_,_,_,_,_,_,_,_),
                          peepsVac1Time(V1),
                          pertence(ID,V1),
                          peepsVac2Time(V2),
                          pertence(ID,V2)),
                      X),
              repRemove(X,B),
              ordena(B, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas não vacinadas; peepsNoVac(Lista de IDs de utentes). -> {V,F}
peepsNoVac(R) :- findall(ID,
                              (utente(ID,_,_,_,_,_,_,_,_,_),
                              peepsVac(V),
                              nao(pertence(ID, V))),
                         X),
                 repRemove(X,B),
                 ordena(B, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas que tem a primeira toma prevista; peepsVac1Futura(lista de IDs de utentes). -> {V,F}
peepsVac1Futura(R) :- findall(ID,
                                    (vacinacao(_,ID,D,M,A, _, 1),
                                     utente(ID,_,_,_,_,_,_,_,_,_),
                                     nao(jaPassou(D, M, A))),
                             X),
                     repRemove(X,B),
                     ordena(B,R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas que tem a segunda toma prevista; peepsVac2Futura(lista de IDs de utentes). -> {V,F}
peepsVac2Futura(R) :- findall(ID,
                                    (vacinacao(_,ID,D,M,A, _, 2),
                                    utente(ID,_,_,_,_,_,_,_,_,_),
                                    nao(jaPassou(D, M, A))),
                              X),
                      repRemove(X,B),
                      ordena(B,R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas não vacinadas e não tem toma prevista; peepsNoVacFutura(lista de IDs de utentes). -> {V,F}
peepsNoVacFutura(R) :- findall(ID,
                                (utente(ID,_,_,_,_,_,_,_,_,_),
                                peepsNoVac(X),
                                pertence(ID,X),
                                peepsVac1Futura(Y),
                                nao(pertence(ID,Y)),
                                peepsVac2Futura(Z),
                                nao(pertence(ID,Z))),
                         A),
                        repRemove(A,B),
                        ordena(B, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas vacianadas indevidamente; indevidamente(lista de IDs de utentes). -> {V,F}
indevidamente(R) :- findall(ID,
                                      (vacinacao(_, ID, D, M, A,_,_),
                                      utente(ID,_,_,_,_,_,_,_,_,_),
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
                                      (utente(ID,_,_,_,_,_,_,_,_,_),
                                      listafaseVacinacao1(F1),
                                      pertence(ID, F1),
                                      peepsVac(V),
                                      nao(pertence(ID, V))),
                              X),
                      repRemove(X,B),
                      ordena(B, R).

%---------------------------------------------------------------------
% Identificar IDs de pessoas que falta a segunda toma da vacina; falta2toma(lista de IDs de utentes). -> {V,F}
falta2toma(R) :- findall(ID,
                                  (utente(ID,_,_,_,_,_,_,_,_,_),
                                  peepsVac1Time(V1),
                                  pertence(ID, V1),
                                  peepsVac2Time(V2),
                                  nao(pertence(ID, V2))),
                              X),
                      repRemove(X,B),
                      ordena(B, R).

%---------------------------------------------------------------------
% estimativa de doses futuras para cada uma das fases
calculaDosesFuturas(Res1,Res2) :- peepsVac1Futura(R1), peepsVac2Futura(R2), length(R1,X1), length(R2,X2), Res1 is X1, Res2 is X2.


%---------------------------------------------------------------------
% estimativa de doses futuras por vacina dada
estimativaPorVacinas(Tipo, Res) :- findall(Tipo, (vacinacao(_,_,D,M,A,Tipo,_), nao(jaPassou(D,M,A))), R), length(R, X), Res is X.

%---------------------------------------------------------------------
% Extensao do meta-predicado nao: Questao -> {V,F}
nao( Questao ) :- Questao, !, fail.
nao( _ ).

%---------------------------------------------------------------------
% sistema de inferência que permite reconhecer se é V/F ou desconhecido
si(Questao,verdadeiro) :- Questao.
si(Questao,falso) :- -Questao.
si(Questao,desconhecido) :- nao(Questao), nao(-Questao).

% meta-redicado conjuncao:
% Q1, Q2, Resposta -> {V,F, D}
conjuncao( verdadeiro, verdadeiro, verdadeiro ).
conjuncao( falso, _, falso ).
conjuncao( _, falso, falso ).
conjuncao( desconhecido, verdadeiro, desconhecido ).
conjuncao( V, desconhecido, desconhecido ) :- V \= falso.

% meta-redicado disjuncao:
% Q1, Q2, Resposta -> {V,F, D}
disjuncao( verdadeiro, _, verdadeiro ).
disjuncao( _, verdadeiro, verdadeiro ).
disjuncao( desconhecido, V, desconhecido ) :- V \= verdadeiro.
disjuncao( V, desconhecido, desconhecido ) :- V \= verdadeiro.
disjuncao( falso, falso, falso ).

% meta-predicado siLista:
% Lista de questoes, Lista de respostas -> {V,F, D}
siLista([],[]).
siLista([Q|T],[R|S]) :- si(Q,R), siLista(T,S).

% meta-predicado siComposicao
% Composicao de questoes, esposta -> {V,F, D}
siComposto(Q && C, R) :- si(Q,RQ), siComposto(C,RC), conjuncao(RQ,RC,R).
siComposto(Q $$ C, R) :- si(Q,RQ), siComposto(C,RC), disjuncao(RQ,RC,R).
siComposto(Q, R) :- si(Q,R).

%--------------------------------------------------------
% --------------- Invariantes de involução --------------
% -------------------------------------------------------
% -------------------------------------------------------

% Pressupostos do mundo fechado
-Q :- nao(Q), nao(excecao(Q)).

% ------------------------------------------------------
% --------------- Conhecimento Incerto -----------------
% ------------------------------------------------------
excecao(utente(Id,_,N,Dt,E,Tlf,M,P,DC,Cs)):- utente(Id,desconhecido,N,Dt,E,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,_,Dt,E,Tlf,M,P,DC,Cs)):- utente(Id,Ss,desconhecido,Dt,E,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,N,_,E,Tlf,M,P,DC,Cs)):- utente(Id,Ss,N,desconhecido,E,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,_,Tlf,M,P,DC,Cs)):- utente(Id,Ss,N,Dt,desconhecido,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,_,M,P,DC,Cs)):- utente(Id,Ss,N,Dt,E,desconhecido,M,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,_,P,DC,Cs)):- utente(Id,Ss,N,Dt,E,Tlf,desconhecido,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,M,_,DC,Cs)):- utente(Id,Ss,N,Dt,E,Tlf,M,desconhecido,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,M,P,_,Cs)):- utente(Id,Ss,N,Dt,E,Tlf,M,P,desconhecido,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,_)):- utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,desconhecido).

excecao(centrosaude(Id,_,M,Tlf,E)) :- centrosaude(Id,desconhecido,M,Tlf,E).
excecao(centrosaude(Id,N,_,Tlf,E)) :- centrosaude(Id,N,desconhecido,Tlf,E).
excecao(centrosaude(Id,N,M,_,E)) :- centrosaude(Id,N,M,desconhecido,E).
excecao(centrosaude(Id,N,M,Tlf,_)) :- centrosaude(Id,N,M,Tlf,desconhecido).

excecao(staff(Id,_,N,E)) :- staff(Id,desconhecido,N,E).
excecao(staff(Id,Cs,_,E)) :- staff(Id,Cs,desconhecido,E).
excecao(staff(Id,Cs,N,_)) :- staff(Id,Cs,N,desconhecido).

excecao(enfermeiro(Id,_,I,G,Cs)) :- enfermeiro(Id,desconhecido,I,G,Cs).
excecao(enfermeiro(Id,N,_,G,Cs)) :- enfermeiro(Id,N,desconhecido,G,Cs).
excecao(enfermeiro(Id,N,I,_,Cs)) :- enfermeiro(Id,N,I,desconhecido,Cs).
excecao(enfermeiro(Id,N,I,G,_)) :- enfermeiro(Id,N,I,G,desconhecido).

excecao(medico(Id,_,I,G,Cs)) :- medico(Id,desconhecido,I,G,Cs).
excecao(medico(Id,N,_,G,Cs)) :- medico(Id,N,desconhecido,G,Cs).
excecao(medico(Id,N,I,_,Cs)) :- medico(Id,N,I,desconhecido,Cs).
excecao(medico(Id,N,I,G,_)) :- medico(Id,N,I,G,desconhecido).

excecao(vacinacao(_,B,C,D,E,F,G)) :- vacinacao(desconhecido,B,C,D,E,F,G).
excecao(vacinacao(A,_,C,D,E,F,G)) :- vacinacao(A,desconhecido,C,D,E,F,G).
excecao(vacinacao(A,B,_,D,E,F,G)) :- vacinacao(A,B,desconhecido,D,E,F,G).
excecao(vacinacao(A,B,C,_,E,F,G)) :- vacinacao(A,B,C,desconhecido,E,F,G).
excecao(vacinacao(A,B,C,D,_,F,G)) :- vacinacao(A,B,C,D,desconhecido,F,G).
excecao(vacinacao(A,B,C,D,E,_,G)) :- vacinacao(A,B,C,D,E,desconhecido,G).
excecao(vacinacao(A,B,C,D,E,F,_)) :- vacinacao(A,B,C,D,E,F,desconhecido).

% ------------------------------------------------------
% -------------- Conhecimento Impreciso ----------------
% ------------------------------------------------------
% Dúvida sobre o nome do utente e morada cujo identificador é 23.
excecao(utente(23, 984735620, 'Maria Marques', 2000, 'marymar@gmail.com', 939273829, porto, estudante, [],16)).
excecao(utente(23, 984735620, 'Maria Marques', 2000, 'marymar@gmail.com', 939273829, braga, estudante, [],16)).
excecao(utente(23, 984735620, 'Mario Marques', 2000, 'marymar@gmail.com', 939273829, porto, estudante, [],16)).
excecao(utente(23, 984735620, 'Mario Marques', 2000, 'marymar@gmail.com', 939273829, braga, estudante, [],16)).
impreciso(23, utente).

% Dúvida sobre a profissão do utente, cujo identificador é 24.
excecao(utente(24, 984735620, 'Maria Abelha', 1990, 'mariabel@gmail.com', 123566778, porto, estudante, [],20)).
excecao(utente(24, 984735620, 'Maria Abelha', 1990, 'mariabel@gmail.com', 123566778, braga, revendedora, [],20)).
impreciso(24, utente).

% Duvida sobre o telefone associado a um centro de saúde cujo identificador é 21.
excecao(centrosaude(21, 'Centro de Saúde de Ruães', 'R. de Ruães, 4700-565 Mire de Tibães', 253602490, 'csruaes@gmail.com')).
excecao(centrosaude(21, 'Centro de Saúde de Ruães', 'R. de Ruães, 4700-565 Mire de Tibães', 253602400, 'csruaes@gmail.com')).
impreciso(21, centrosaude).

% Duvida sobre o nome do staff cujo identificador é 37.
excecao(staff(37, 25, 'Tiago Borges', 'tiagoborges@gmail.com')).
excecao(staff(37, 25, 'Tiago Rodrigues', 'tiagoborges@gmail.com')).
impreciso(37, staff).

% Duvida sobre a idade do enfermeiro cujo identificador é 21.
excecao(enfermeiro(21,'Sousa Tavares',45,'F',20)).
excecao(enfermeiro(21,'Sousa Tavares',39,'F',20)).
impreciso(21, enfermeiro).

% Duvida sobre o centro de saude e a idade do medico cujo identificador é 21.
excecao(medico(21,'Borges Carvalho',41,'F',19)).
excecao(medico(21,'Borges Carvalho',41,'F',20)).
excecao(medico(21,'Borges Carvalho',45,'F',19)).
excecao(medico(21,'Borges Carvalho',45,'F',20)).
impreciso(21, medico).

% Duvida sobre o tipo da vacinacao.
excecao(vacinacao(25,13,29,06,2021, astraZeneca, 2)).
imprecisoVacinacao(25,13,29,06,2021, vacinacao).

% Duvida sobre a toma da vacinacao.
excecao(vacinacao(25,13,29,06,2021, pfizer, 1)).
imprecisoVacinacao(25,13,29,06,2021, vacinacao).

% ------------------------------------------------------
% -------------- Conhecimento Interdito ----------------
% ------------------------------------------------------
excecao(utente(Id,_,N,Dt,E,Tlf,M,P,DC,Cs)):- utente(Id,semSegurancaSocial,N,Dt,E,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,_,Dt,E,Tlf,M,P,DC,Cs)):- utente(Id,Ss,semNome,Dt,E,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,N,_,E,Tlf,M,P,DC,Cs)):- utente(Id,Ss,N,semIdade,E,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,_,Tlf,M,P,DC,Cs)):- utente(Id,Ss,N,Dt,semEmail,Tlf,M,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,_,M,P,DC,Cs)):- utente(Id,Ss,N,Dt,E,semTelefone,M,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,_,P,DC,Cs)):- utente(Id,Ss,N,Dt,E,Tlf,semMorada,P,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,M,_,DC,Cs)):- utente(Id,Ss,N,Dt,E,Tlf,M,semProfissao,DC,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,M,P,_,Cs)):- utente(Id,Ss,N,Dt,E,Tlf,M,P,semDoencasCronicas,Cs).
excecao(utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,_)):- utente(Id,Ss,N,Dt,E,Tlf,M,P,DC,semCentroSaude).

excecao(centrosaude(Id,_,M,Tlf,E)) :- centrosaude(Id,semNome,M,Tlf,E).
excecao(centrosaude(Id,N,_,Tlf,E)) :- centrosaude(Id,N,semMorada,Tlf,E).
excecao(centrosaude(Id,N,M,_,E)) :- centrosaude(Id,N,M,semTelefone,E).
excecao(centrosaude(Id,N,M,Tlf,_)) :- centrosaude(Id,N,M,Tlf,semEmail).

excecao(staff(Id,_,N,E)) :- staff(Id,semCentroSaude,N,E).
excecao(staff(Id,Cs,_,E)) :- staff(Id,Cs,semNome,E).
excecao(staff(Id,Cs,N,_)) :- staff(Id,Cs,N,semEmail).

excecao(enfermeiro(Id,_,I,G,Cs)) :- enfermeiro(Id,semNome,I,G,Cs).
excecao(enfermeiro(Id,N,_,G,Cs)) :- enfermeiro(Id,N,semIdade,G,Cs).
excecao(enfermeiro(Id,N,I,_,Cs)) :- enfermeiro(Id,N,I,semGenero,Cs).
excecao(enfermeiro(Id,N,I,G,_)) :- enfermeiro(Id,N,I,G,semCentroSaude).

excecao(medico(Id,_,I,G,Cs)) :- medico(Id,semNome,I,G,Cs).
excecao(medico(Id,N,_,G,Cs)) :- medico(Id,N,semIdade,G,Cs).
excecao(medico(Id,N,I,_,Cs)) :- medico(Id,N,I,semGenero,Cs).
excecao(medico(Id,N,I,G,_)) :- medico(Id,N,I,G,semCentroSaude).

excecao(vacinacao(_,B,C,D,E,F,G)) :- vacinacao(semStaff,B,C,D,E,F,G).
excecao(vacinacao(A,_,C,D,E,F,G)) :- vacinacao(A,semUtente,C,D,E,F,G).
excecao(vacinacao(A,B,_,D,E,F,G)) :- vacinacao(A,B,semDia,D,E,F,G).
excecao(vacinacao(A,B,C,_,E,F,G)) :- vacinacao(A,B,C,semMes,E,F,G).
excecao(vacinacao(A,B,C,D,_,F,G)) :- vacinacao(A,B,C,D,semAno,F,G).
excecao(vacinacao(A,B,C,D,E,_,G)) :- vacinacao(A,B,C,D,E,semTipoVacina,G).
excecao(vacinacao(A,B,C,D,E,F,_)) :- vacinacao(A,B,C,D,E,F,semToma).

nulo(semIdade).
nulo(semMorada).
nulo(semAno).
nulo(semMes).
nulo(semDia).
nulo(semSegurancaSocial).
nulo(semNome).
nulo(semTelefone).
nulo(semEmail).
nulo(semProfissao).
nulo(semDoencasCronicas).
nulo(semCentroSaude).
nulo(semGenero).
nulo(semStaff).
nulo(semUtente).
nulo(semTipoVacina).
nulo(semToma).

% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% --------------------------- Auxiliares ------------------------------
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------

%---------------------------------------------------------------------
% Verifica se uma data é anterior à atual; jaPassou(Dia, Mes, Ano). -> {V,F}
jaPassou(_, _, A) :- dataA(_,_, Aatual),
                     A < Aatual.
jaPassou( _, M, A) :- dataA(_, Matual, Aatual),
                      A =:= Aatual,
                      M < Matual.
jaPassou(D, M, A) :- dataA(Datual, Matual, Aatual),
                     A =:= Aatual,
                     M =:= Matual,
                     D < Datual.

%---------------------------------------------------------------------
% Verifica se um utente ja tomou as duas doses da vacina; veraux(ID do utente). -> {V,F}
veraux(ID) :- utente(ID,_,_,_,_,_,_,_,_,_),
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
ordena([],[]).
ordena([X],[X]).
ordena([H|T],F):- ordena(T,N), inserec(H,N,F).

%---------------------------------------------------------------------
% Remove elems repetidos de uma lista; repRemove(lista de elementos, lista de elementos). -> {V,F}
repRemove([],[]).
repRemove([X|A],R) :- elemRemove(X,A,L),
                      repRemove(L,T),
                      R = [X|T].

%---------------------------------------------------------------------
% Remove todos os elems; removeAll(lista de elementos). -> {V,F}
removeAll([H|T]):- remove(H), removeAll(T).
removeAll([]).              

%---------------------------------------------------------------------
% Remove a primeira ocurrencia de um elem numa lista; elemRemove(elemento, lista de elementos, lista de elementos). -> {V,F}
elemRemove(_,[],[]).
elemRemove(A,[A|Y],T) :- elemRemove(A,Y,T).
elemRemove(A,[X|Y],T) :- X \== A,
                         elemRemove(A,Y,R),
                      	 T = [X|R].

%---------------------------------------------------------------------
% Comparar duas datas. Data 1 + Data 2
comparaDatas(_,_,A1,_,_,A2) :- A1 < A2.
comparaDatas(_,M1,A1,_,M2,A2) :- M1 =< M2, A1 =:= A2.
comparaDatas(D1,M1,A1,D2,M2,A2) :- D1 =< D2, M1 =:= M2, A1 =:= A2.
