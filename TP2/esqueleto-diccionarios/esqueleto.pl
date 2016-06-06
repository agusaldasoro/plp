:- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.


% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).





% Ejercicio 1 %
%% diccionario_lista(?C) si hay un diccionario cargado.
diccionario_lista(C) :- diccionario(X), string_codes(X, C).


% Ejercicio 2 %
% juntar_con(?L, ?J, ?R), pero dos tienen que estar instanciados.
% Asumimos que si J aparece en L, entonces es porque es el caracter de corte.
% Es decir, no se puede ingresar una lista dentro de L que contenga a J.
% Si aparece J dos veces seguidas en R, es porque hubo una lista vacía en el medio.
juntar_con([], _, []).
juntar_con([L], J, L) :- not(member(J, L)).
juntar_con([H | [L | LS]], J, R) :- append(H, [J], Y), append(Y, P, R),
                                     juntar_con([L | LS], J, P), !.


% Ejercicio 3 %
%% palabras(?S, ?P)
palabras(S, P) :- juntar_con(P, espacio, S), !.


% Ejercicio 4 %
% asignar_var(+A, +MI, ?MF) por el contexto de uso especificado en el enunciado.
% Si la variable ya esta mapeada, se asigna a MF el mismo diccionario MI.
asignar_var(A, MI, MI) :- esta_mapeada(A, MI). 
% Sino, MF es el resultado de agregar A a MI con algun valor (_). 
asignar_var(A, MI, MF) :- not(esta_mapeada(A, MI)), append(MI, [(A, _)], MF).

% Para ver si esta mapeada, se pregunta si existe la tupla (A, _)
% es decir, figura A mapeada con algun valor (_) en MI.
esta_mapeada(A, MI) :- member((A, _), MI).


% Ejercicio 5 %
%% palabras_con_variables(+P, ?V)
%% P esta instanciada por el contexto del enunciado, V solo puede estar instanciada si P lo esta.
palabras_con_variables(P, V) :- flatten(P, Y), asignar_variables(Y, MF), dividir(P, MF, V), !.

%% dividir(+PS, +MF, ?VS) estan instanciadas por su contexto de uso.
dividir([], _, []).
dividir([P | PS], MF, [V | VS]) :- cambiar_a_variables(P, MF, V), dividir(PS, MF, VS).

%% cambiar_a_variables(+AS, +MF, ?VS) estan instanciadas por su contexto de uso.
cambiar_a_variables([], _, []).
cambiar_a_variables([A | AS], MF, [V | VS]) :- member((A, V), MF), cambiar_a_variables(AS, MF, VS).

%% asignar_variables(+PS, ?MI) PS esta instanciada por el contexto del enunciado
asignar_variables([], _).
asignar_variables([P | PS], MI) :- asignar_var(P, MI, MF), asignar_variables(PS, MF).


% Ejercicio 6 %
%% quitar(+E, +LS, ?R)
quitar(_, [], []).
quitar(E, [Y | LS], R) :- Y == E, quitar(E, LS, R).
quitar(E, [L | LS], [L | R]) :- L \== E, quitar(E, LS, R).


% Ejercicio 7 %
%% cant_distintos(+XS, ?N)
cant_distintos([], 0).
cant_distintos([X | XS], N) :- quitar(X, XS, Y), cant_distintos(Y, P), N is P + 1.


% Ejercicio 8 %
%% descifrar(?S, ?M) M puede estar instanciada solo si S lo esta
descifrar(S, M) :- palabras(S, P), palabras_con_variables(P, V), encontrar_codigos(V),
                    cant_distintos(P, N1), cant_distintos(V, N2), N1 is N2, pasar_a_string(V, M).

%% encontrar_codigos(?VS)
encontrar_codigos([]).
encontrar_codigos([V | VS]) :- encontrar_codigos(VS), diccionario_lista(V).

%% pasar_a_string(?X, ?Y)
pasar_a_string(X, Y) :- juntar_con(X, 32, Z), string_codes(Y, Z).


% Ejercicio 9 %
%% descifrar_sin_espacios(+A, ?B)
descifrar_sin_espacios(A, B) :- generar_listas(A, C), descifrar(C, B).

%% generar_listas(?AS, ?BS) Si BS esta instanciada y AS no,
%% entonces le asigna a AS el caracter espacio, que asumimos como pre condicion que AS no lo tenia.
generar_listas([], []).
generar_listas([A | AS], [A | BS]) :- poner_espacios(AS, BS).

%% poner_espacios(?AS, ?BS)
poner_espacios([], []).
poner_espacios([A | AS], [espacio | [A | BS]]) :- poner_espacios(AS, BS).
poner_espacios([A | AS], [A | BS]) :- poner_espacios(AS, BS).


% Ejercicio 10 %
%% mensajes_mas_parejos(+S, ?M)
mensajes_mas_parejos(S, M):- descifrar_sin_espacios(S, M), es_minimo(S, M).

%% es_minimo(+S, ?M)
es_minimo(S, M) :- not(existe_menor(S, M)).

%% existe_menor(+S, ?M)
existe_menor(S, M) :- descifrar_sin_espacios(S, N), desvio_standard(M, A), desvio_standard(N, B),
                       A > B.

%% desvio_standard(+M, ?P)
desvio_standard(M,P) :- string_codes(M, C), juntar_con(L,32, C), tamanio_promedio(L, N),
                         distancia_al_promedio(L, N, Q), length(L, R), P = sqrt(Q/R).

%% distancia_al_promedio(+LS, +N, ?P)
distancia_al_promedio([], _, 0).
distancia_al_promedio([L | LS] , N, P) :- length(L, M), distancia_al_promedio(LS, N, O),
                                           P is O + (M- N) * (M - N).

%% tamanio_promedio(+L, ?P)
tamanio_promedio([], 0).
tamanio_promedio(L, P) :- suma_tamanos(L,O), length(L, N), P is O/N.

%% suma_tamanos(+LS, ?P)
suma_tamanos([], 0).
suma_tamanos([L | LS], P) :- length(L, M), suma_tamanos(LS, O), P is M + O.
