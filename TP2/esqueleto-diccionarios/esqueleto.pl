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
diccionario_lista(C) :- diccionario(X), string_codes(X, C).

% Ejercicio 2 %
% juntar_con(?L, ?J, ?R), pero dos tienen que estar instanciados.
% Asumimos que si J aparece el L, entonces es porque es el caracter de corte.
% Es decir, no se puede ingresar una lista dentro de L que contenga a J.
% Si aparece J dos veces seguidas en R, es xq hubo una lista vacía en el medio.
juntar_con([], _, []).
juntar_con([X], C, X) :- not(member(C, X)).
juntar_con([H | LS], C, L) :- append(H, [C], Y), sacar_prefijo(Y, L, P), juntar_con(LS, C, P).

sacar_prefijo(H, L, P) :- append(H, P, L).

% Ejercicio 3 %
palabras(S, P) :- juntar_con(P, espacio, S).

% Ejercicio 4 %
% asignar_var(?A, ?MI, ?MF)
% AGREGAR XQ FUNCIONA %
asignar_var(A, MI, MI) :- esta_mapeada(A, MI).
asignar_var(A, MI, MF) :- not(esta_mapeada(A, MI)), append(MI, [(A, _)], MF).

esta_mapeada(A, MI) :- member((A, _), MI).

% Ejercicio 5 %
palabras_con_variables(P, V) :- flatten(P, Y), asignar_variables(Y, MF), dividir(P, MF, V).

dividir([], _, []).
dividir([P | PS], MF, [V | VS]) :- dividir(PS, MF, VS), cambiar_a_variables(P, MF, V).

cambiar_a_variables([], _, []).
cambiar_a_variables([A | AS], MF, [V | VS]) :- member((A, V), MF), cambiar_a_variables(AS, MF, VS).

asignar_variables([], _).
asignar_variables([P | PS], MI) :- asignar_var(P, MI, MF), asignar_variables(PS, MF).

% Ejercicio 6 %
quitar(_, [], []).
quitar(E, [Y | LS], R) :- Y == E, quitar(E, LS, R).
quitar(E, [L | LS], [L | R]) :- L \== E, quitar(E, LS, R).

% Ejercicio 7 %
cant_distintos([], 0).
cant_distintos([X | XS], N) :- quitar(X, XS, Y), cant_distintos(Y, P), N is P + 1.

% Ejercicio 8 %
descifrar(S, M) :- palabras(S, P), palabras_con_variables(P, V), encontrar_codigos(V), pasar_a_string(V, M).

encontrar_codigos([]).
encontrar_codigos([V | VS]) :- diccionario_lista(V), encontrar_codigos(VS).

pasar_a_string(X, Y) :- juntar_con(X, 32, Z), string_codes(Y, Z).

% Ejercicio 9 %
descifrar_sin_espacios(S, M) :- length(S, I), quitar(espacio, R, S), apariciones(espacio, R, C), C =< I, descifrar(S, M).

apariciones(_, [], 0).
apariciones(E, [L | LS], N) :- E == L, apariciones(E, LS, M), N is M + 1.
apariciones(E, [L | LS], N) :- E \== L, apariciones(E, LS, N).


% Ejercicio 10 %
mensajes_mas_parejos(_, _).
