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





% Ejercicio 1 % Resultado OK
diccionario_lista(C) :- diccionario(X), string_codes(X, C).

% Ejercicio 2 % Resultado OK
% juntar_con(?L, ?J, ?R), pero dos tienen que estar instanciados.
% Asumimos que si J aparece el L, entonces es porque es el caracter de corte.
% Es decir, no se puede ingresar una lista dentro de L que contenga a J.
% Si aparece J dos veces seguidas en R, es xq hubo una lista vacía en el medio.
juntar_con([], _, []).
juntar_con([X], C, X) :- not(member(C, X)).
juntar_con([H | [L | LS]], C, R) :- append(H, [C], Y), append(Y, P, R), juntar_con([L | LS], C, P), !.

% Ejercicio 3 % Se cuelga con ; (coincide PDF)
palabras(S, P) :- juntar_con(P, espacio, S).

% Ejercicio 4 % No siempre devuelve false al apretar ; a veces corta ejecucion sin error.
% asignar_var(?A, ?MI, ?MF)
% AGREGAR XQ FUNCIONA %
asignar_var(A, MI, MI) :- esta_mapeada(A, MI).
asignar_var(A, MI, MF) :- not(esta_mapeada(A, MI)), append(MI, [(A, _)], MF).

esta_mapeada(A, MI) :- member((A, _), MI).

% Ejercicio 5 % CHAMO: creo que está bien poner el ! acá, porque como devolvemos variables, siempre hay infinitas soluciones.
palabras_con_variables(P, V) :- flatten(P, Y), asignar_variables(Y, MF), dividir(P, MF, V), !.

dividir([], _, []).
dividir([P | PS], MF, [V | VS]) :- cambiar_a_variables(P, MF, V), dividir(PS, MF, VS).

cambiar_a_variables([], _, []).
cambiar_a_variables([A | AS], MF, [V | VS]) :- member((A, V), MF), cambiar_a_variables(AS, MF, VS).

asignar_variables([], _).
asignar_variables([P | PS], MI) :- asignar_var(P, MI, MF), asignar_variables(PS, MF).

% Ejercicio 6 % Resultado OK
quitar(_, [], []).
quitar(E, [Y | LS], R) :- Y == E, quitar(E, LS, R).
quitar(E, [L | LS], [L | R]) :- L \== E, quitar(E, LS, R).

% Ejercicio 7 % Resultado OK
cant_distintos([], 0).
cant_distintos([X | XS], N) :- quitar(X, XS, Y), cant_distintos(Y, P), N is P + 1.

% Ejercicio 8 % Anda mal, devuelve cosas incorrectas y repetidos.
descifrar(S, M) :- palabras(S, P), palabras_con_variables(P, V), encontrar_codigos(V), pasar_a_string(V, M).

encontrar_codigos([]).
encontrar_codigos([V | VS]) :- encontrar_codigos(VS), flatten(VS, VSS), diccionario_lista(V), disjuntos(V, VSS).

disjuntos(XS, YS) :- cant_distintos(XS, N1), cant_distintos(YS, N2), append(XS, YS, ZS), cant_distintos(ZS, N3), N3 is N1 + N2.

pasar_a_string(X, Y) :- juntar_con(X, 32, Z), string_codes(Y, Z).

% Ejercicio 9 % Se cuelga, no devuelve ningun resultado.
descifrar_sin_espacios(A, B) :- generar_listas(A, C), descifrar(C, B).

%% generar_listas es reversible. generar_listas: Resultado OK
generar_listas([A | AS], [B | BS]) :- A = B, B \= espacio, poner_espacios(AS, BS).

poner_espacios([], []).
poner_espacios([A | AS], [R | [L | LS]]) :- R = espacio, A = L, L \= espacio, poner_espacios(AS, LS).
poner_espacios([A | AS], [L | LS]) :- A = L, L \= espacio, poner_espacios(AS, LS).

% Ejercicio 10 %
mensajes_mas_parejos(_, _).
