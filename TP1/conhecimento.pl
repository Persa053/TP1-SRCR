%---------------------------------------------------------------------
%---------------------------------------------------------------------
%----------------------- Base de Conhecimento ------------------------
%---------------------------------------------------------------------
%---------------------------------------------------------------------


:- module(database,[ utente/10, centrosaude/5, staff/4, vacinacao/7, medico/5, enfermeiro/5]).
:- dynamic utente/10.
:- dynamic centrosaude/5.
:- dynamic staff/4.
:- dynamic vacinacao/7.
:- dynamic medico/5.
:- dynamic enfermeiro/5.


%---------------------------------------------------------------------
%-------- Utente -----------------------------------------------------
%---------------------------------------------------------------------
%Utente: #Idutente, Nº Segurança_Social, Nome, Data_Nasc, Email, Telefone, Morada, Profissão, [Doenças_Crónicas], #CentroSaúde -> {V,F}

%Base de conhecimento do Utente
utente(1, 087462728, 'Jose Oliveira',1965, 'joseoliv@gmail.com', 917263549, braga, engenheirx, [colestrol],1).
utente(2, 528102846, 'Diogo Espirito Santo',1956, 'diant@gmail.com', 936489367, coimbra, medicx, [],7).
utente(3, 658746027, 'Afonso Castro', 1997, 'fonso@gmail.com', 927362918,braga, empresarix, [hipertensao],2).
utente(4, 129837465, 'Duarte Catalao', 1986, 'dudu@gmail.com', 918374526,lisboa, arquitetx,[osteoporose],5).
utente(5, 092183746, 'Lara Vilhena', 1996, 'laravilhena12@gmail.com', 9387362907, porto, modelo, [],8).
utente(6, 394876938, 'Joao Silva', 1994, 'jonysilva@gmail.com', 912653829, guimaraes, futebolista, [],9).
utente(7, 984735620, 'Maria Marques', 2000, 'marymar@gmail.com', 939273829, porto, estudante, [],16).
utente(8, 093847568, 'Sofia Alves', 1998, 'sofsalves4@gmail.com', 918273648,lisboa, enfermeirx, [],6).
utente(9, 498327462, 'Andre Pereira',1995, 'andregoper56@gmail.com', 928756401, porto, cozinheirx, [asma],19).
utente(10, 834768723, 'Rui Vilela', 2001, 'ruivilela@gmail.com', 935468937, barcelos, estudante, [],15).
utente(11, 092134546, 'Veronica Pontes', 2000, 'veronica63@gmail.com', 918273648, guimaraes, estudante, [],3).
utente(12, 398475394, 'Francisca Avila',1954, 'franciscavila@gmail.com', 938476032, braga, medicx, [alzheimer],3).
utente(13, 298374563, 'Pedro Gouveia', 1967, 'pedrogouveia@gmail.com', 928354082, lisboa, professorx, [],13).
utente(14, 138473578, 'Sofia Alves', 1972, 'sofsalves4@gmail.com', 937485926, fafe, engenheirx, [diabetes],1).
utente(15, 578328474, 'Margarida Maia', 1988, 'margarida_m@gmail.com', 923457366, aveiro, terapeuta, [asma],10).
utente(16, 873645634, 'Ana Lopes', 1958, 'analopes@gmail.com', 912375648, braga, professor, [diabetes,rinite],13).
utente(17, 337462863, 'Dalila Pavao', 1965, 'dalilapavao@gmail.com', 937593746, porto, engenheirx, [parkinson],14).
utente(18, 163874673, 'Manuela Rodrigues', 1977, 'manuelarodrigues@gmail.com', 912847562, guimaraes, policia, [],20).
utente(19, 098475833, 'Jose Soares', 1989, 'josesoaresvilela@gmail.com', 937264839, faro, padeirx, [],4).
utente(20, 763487435, 'Luis Veloso', 1978, 'luisveloso@gmail.com', 927465839, aveiro, marinheirx, [],7).
utente(21, 712345325, 'Alberto Sousa', 1800, 'als@gmail.com', 912344539, ruilhe, advogadx, [],2).
utente(22, 347086589, 'Joao Baiao', 1930, 'joaos123@gmail.com', 915479768, braga, padeirx, [],11).


%---------------------------------------------------------------------
%-------- CentroSaude ------------------------------------------------
%---------------------------------------------------------------------
%centro_saúde: #Idcentro, Nome, Morada, Telefone, Email -> {V,F}

%Base de conhecimento dos Centros de Saúde 
centrosaude(1, 'Centro de Saúde de Braga', 'Largo Paulo Orósio, 4700-031 Braga', 253928647, 'csbraga@gmail.com').
centrosaude(2, 'USF Bracara Augusta', 'Praça Gen. Humberto Delgado 47, 4715-213 Braga', 253964876, 'bracaraaugusta@gmail.com').
centrosaude(3, 'USF Sanus Caranda', 'R. André Soares 25, 4715-213 Braga', 253201530, 'usfcaranda@gmail.com').
centrosaude(4, 'Centro de Saúde de Guimarães', 'R. Francisco Santos Guimarães, 4810-225 Guimarães', 253519923, 'csguimaraes@gmail.com').
centrosaude(5, 'Centro de Saude Amorosa', 'R. José Pinto Rodrigues 16, Guimarães, 4810-225 Guimarães', 253421340, 'csamorosa@gmail.com').
centrosaude(6, 'Unidade de Saúde Familiar Cruz de Celas', 'Av. Dom Afonso Henriques 141, 3000-011 Coimbra', 239488261, 'usfcelas@gmail.com').
centrosaude(7, 'USCP Fernão de Magalhães', 'Av. Fernão de Magalhães 620, 3000-174 Coimbra', 239856110, 'uscpfernaomagalhaes@gmail.com').
centrosaude(8, 'Centro de Saúde Norton de Matos', '3030-790 Coimbra', 239794110, 'csnorton@gmail.com').
centrosaude(9, 'Centro de Saúde da Penha de França', 'R. Luís Pinto Moitinho 5, Lisboa', 218164100, 'cspenhadefranca@gmail.com').
centrosaude(10, 'Centro de Saúde de Sete Rios', 'R. São Domingos de Benfica 20, Lisboa', 2172171800, 'csseterios@gmail.com').
centrosaude(11, 'Centro de Saúde da Lapa', 'R. São Domingos de Benfica 20, Lisboa', 217211800, 'cslapa@gmail.com').
centrosaude(12, 'Centro Saúde do Lumiar', '5°/6°, 144, Alameda das Linhas de Torres 243, Lisboa', 217527110, 'cslumiar@gmail.com').
centrosaude(13, 'USF Porto Centro', 'Rua de Santos Pousada 298, 4000-478 Porto', 225360060, 'usfporto@gmail.com').
centrosaude(14, 'Centro de Saúde de Paranhos', '510, R. de Vale Formoso 466, Porto', 22 834 7355, 'csparanhos@gmail.com').
centrosaude(15, 'Centro de Saúde do Bonfim', 'R. do Barão de Nova Sintra 244, 4300-367 Porto', 225898560, 'csbonfim@gmail.com').
centrosaude(16, 'USF Porto Centro', 'Rua de Santos Pousada 298, 4000-478 Porto', 225360060, 'usfporto@gmail.com').
centrosaude(17, 'Centro de Saúde de Faro', 'Urbanização Hortas das Figuras, 8009-003 Faro', 289830300, 'csfaro@gmail.com').
centrosaude(18, 'USF Novo Cuidar', 'R. José Ribeiro Vieira de Castro 125, 4820-273 Fafe', 253490863, 'usffafe@gmail.com').
centrosaude(19, 'Centro de Saúde de Aveiro', 'Viela da Fonte dos Amores 57A, 3810-164 Aveiro', 234891170, 'csaveiro@gmail.com').
centrosaude(20, 'Centro de Saúde de Barcelos', 'R. Dr. Abel Varzim, 4750-253 Barcelos', 253808300, 'csbarcelos@gmail.com').


%---------------------------------------------------------------------
%-------- CentroSaude ------------------------------------------------
%---------------------------------------------------------------------
%staff: #Cstaff, #Idcentro, Nome, email -> {V,F}

%Base de conhecimento do staff
staff(1, 1, 'Monica Sintra','monicas@gmail.com').
staff(2, 1, 'Cristiano Alves', 'cristianoalves@gmail.com').
staff(3, 1, 'Jorge Pires', 'jorgeemanuel@gmail.com').
staff(4, 2, 'Antonio Goncalves', 'antoniogoncalves@gmail.com').
staff(5, 2, 'Marco Barbosa', 'marquitxi@gmail.com').
staff(6, 2, 'Sara Oliveira', 'sarinha@gmail.com').
staff(7, 3, 'Joana Marques', 'joanamarques@gmail.com').
staff(8, 3, 'Emanuel Barros', 'emanuelbarros@gmail.com').
staff(9, 4, 'Paulo Rodrigues', 'paulinho@gmail.com').
staff(10, 4, 'Tiago Silva', 'tiagosilva@gmail.com').
staff(11, 5, 'Anabela Alves', 'anabelalves@gmail.com').
staff(12, 5, 'Vitor Moutinho', 'vitor39@gmail.com').
staff(13, 6, 'Hugo Martins', 'hugo@gmail.com').
staff(14, 6, 'Rita Noronha', 'rita@gmail.com').
staff(15, 7, 'Diogo Alves', 'diogo57@gmail.com').
staff(16, 7, 'Mateus Jordao', 'mateujord@gmail.com').
staff(17, 8, 'Manuel Ferreira', 'manuelferr@gmail.com').
staff(18, 8, 'Mariana Vieira', 'marianavieira@gmail.com').
staff(19, 9, 'Lucas Barbosa', 'lucasbarb@gmail.com').
staff(20, 9, 'Salvador Campos', 'salvadorca@gmail.com').
staff(21, 10, 'Guilherme Falcao', 'guilhermefalcao@gmail.com').
staff(22, 10, 'Sofia Mendes', 'sofiamendes@gmail.com').
staff(23, 10, 'Henrique Paz', 'henrique@gmail.com').
staff(24, 11, 'Micaela Carreira', 'micaela@gmail.com').
staff(25, 12, 'Ana Rodrigues', 'anarodri@gmail.com').
staff(26, 12, 'Maria Miguel Lopes', 'mariamig@gmail.com').
staff(27, 13, 'Filomena Cardoso', 'cardoso@gmail.com').
staff(28, 14, 'Miguel Figueiredo', 'miguelfigueiredo@gmail.com').
staff(29, 14, 'Tomas Quintas', 'tomasquintas@gmail.com').
staff(30, 15, 'Ana Jorge', 'anajor@gmail.com').
staff(31, 15, 'Jose Pedro Quintas', 'josepedro@gmail.com').
staff(32, 16, 'Rita Negroes', 'ritanegroes@gmail.com').
staff(33, 17, 'Jorge Borges', 'jorgeborges@gmail.com').
staff(34, 18, 'Fernando Santos', 'fernandosantos@gmail.com').
staff(35, 19, 'Paulo Filipe', 'paulpfilipe@gmail.com').
staff(36, 20, 'Filipa Trindade', 'filipatrindade@gmail.com').

%---------------------------------------------------------------------
%-------- Vacinação --------------------------------------------------
%---------------------------------------------------------------------
%vacinação_Covid: #Staf, #utente, Dia, Mes,Ano, Vacina, Toma -> {V,F}

%Base de conhecimento da vacinação
vacinacao(1,1,02,05,2021, astraZeneca, 1).
vacinacao(13,2,20,04,2021, pfizer, 1).
vacinacao(4,3,31,09,2021, pfizer, 1).
vacinacao(20,4,18,02,2021, pfizer, 2).
vacinacao(26,5,09,10,2021, pfizer, 2).
vacinacao(9,6,13,02,2022, astraZeneca, 1).
vacinacao(28,7,25,01,2022, astraZeneca, 2).
vacinacao(24,8,01,04,2021, pfizer, 2).
vacinacao(32,9,30,01,2021, pfizer, 2).
vacinacao(36,10,05,09,2021, pfizer, 1).
vacinacao(12,11,26,12,2021, astraZeneca, 2).
vacinacao(8,12,23,03,2021, pfizer, 1).
vacinacao(25,13,29,06,2021, pfizer, 2).
vacinacao(34,14,20,06,2021, astraZeneca, 1).
vacinacao(35,15,07,01,2022, pfizer, 1).
vacinacao(6,16,31,05,2021, pfizer, 1).
vacinacao(31,17,19,07,2021, pfizer, 2).
vacinacao(10,18,21,07,2021, astraZeneca, 1).
vacinacao(33,19,21,11,2021, astraZeneca, 1).
vacinacao(35,20,13,08,2021, pfizer, 1).

%---------------------------------------------------------------------
%-------- MedicoFamilia ----------------------------------------------
%---------------------------------------------------------------------
% medicoFamilia: #IdMedico,Nome,Idade,Género,#CentroSaude -> {V,F}

%Base de conhecimento dos medicos
medico(1,'Ester Domingues',47,'F',1).
medico(2,'Manuel Castro',29,'M',2).
medico(3,'Emanuel Anjo',38,'M',3).
medico(4,'David Ferreira',43,'M',4).
medico(5,'Pedro Melo',40,'M',5).
medico(6,'Ana Rita Nobre',38,'F',6).
medico(7,'Fábio Gomes',56,'M',7).
medico(8,'Renato Mendes',34,'M',8).
medico(9,'Jose Cracel',61,'M',9).
medico(10,'Catia Gomes',28,'F',10).
medico(11,'Luisa Veloso',33,'F',11).
medico(12,'Marta Pereira',47,'F',12).
medico(13,'Leonor Gonçalves',39,'F',13).
medico(14,'André Fernandes',30,'M',14).
medico(15,'Duarte Pinto',55,'M',15).
medico(16,'Maria Santos',40,'F',16).
medico(17,'Marco Marques',46,'M',17).
medico(18,'Isabel Arantes',58,'F',18).
medico(19,'Duarte Teixeira',44,'M',19).
medico(20,'Sara Carvalho',41,'F',20).





%---------------------------------------------------------------------
%-------- Enfermeiro -------------------------------------------------
%---------------------------------------------------------------------
% enfermeiro: #IdEnfermeiro,Nome,Idade,Género,#CentroSaude -> {V,F}

%Base de conhecimento dos enfermeiros
enfermeiro(1,'Márcia Araujo',27,'F',1).
enfermeiro(2,'Luzia Gomes',41,'F',2).
enfermeiro(3,'Mafalda Araujo',50,'F',3).
enfermeiro(4,'Henrique Megre',36,'M',4).
enfermeiro(5,'Daniela Sousa',53,'F',5).
enfermeiro(6,'Sara Maia',25,'F',6).
enfermeiro(7,'Beatriz Silva',46,'F',7).
enfermeiro(8,'Gabriela Simoes',50,'F',8).
enfermeiro(9,'Alexandre Castro',30,'M',9).
enfermeiro(10,'Ines Pitrez',55,'F',10).
enfermeiro(11,'Eduarda Martins',26,'F',11).
enfermeiro(12,'Rui Lourenço',37,'M',12).
enfermeiro(13,'Henrique Faria',54,'M',13).
enfermeiro(14,'Eduarda Azevedo',38,'F',14).
enfermeiro(15,'Jorge Handel',44,'M',15).
enfermeiro(16,'Marta Enes',47,'F',16).
enfermeiro(17,'Daniela Pinto',52,'F',17).
enfermeiro(18,'Leonor Taborda',48,'F',18).
enfermeiro(19,'Janico Abreu',34,'M',19).
enfermeiro(20,'Diana Vieira',39,'F',20).