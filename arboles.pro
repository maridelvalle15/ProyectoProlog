%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% ÁRBOLES COMO ESTRUCTURAS %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 
	Verificamos que se cumpla e=|a-b| donde: 
		C es el nodo padre 
		A es el valor de la arista
		B es el valor del nodo hijo
*/
resta_abs(A, B, C):- A is abs(C - B).

/* 	
	Verificamos que el valor de cada nodo no sea mayor a la cantidad de nodos en el arbol
	Verificamos que el valor de cada arista no sea mayor a la cantidad de aristas en el arbol
*/
verificarNodosAristas([],_).
verificarNodosAristas([C|L],T) :- C =< T, verificarNodosAristas(L,T).

/* 
	Recorremos el arbol y verificamos:
	El valor de los nodos y las aristas debe ser un numero entero mayor a cero
	Verificamos la suma de las etiquetas
	Adicionalmente almacenamos los valores de los nodos en una lista
	Adicionalmente almacenamos los valores de las aristas en una lista
*/

nodoAux(C,[],L,[C|L],Ar,Ar).

nodoAux(C,[arista(A,nodo(B,L))|NS],R,Raux,Ar,ArAux):- C>0, A>0, B>0, integer(C), integer(A), integer(B), resta_abs(A,B,C), append([A],Ar3,ArAux), nodoAux(C,NS,R2,Raux,Ar2,Ar3), nodoAux(B,L,R,R2,Ar,Ar2).

/* 
	Llamamos al predicado para verificar que el arbol este bien etiquetado:
	Verificamos que los elementos de la lista de nodos no se repitan
*/
bienEtiquetado(nodo(C,L)) :- nodoAux(C,L,[],X,[],A), is_set(X), is_set(A), length(X,T), verificarNodosAristas(X,T), length(A,M), verificarNodosAristas(A,M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% ÁRBOLES COMO LISTAS %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% CREAR ESQUELETO PARA FORMAR ARBOL R-ARIO %%%

/* 
	Contamos el numero de nodos para el ultimo nivel
*/
contarNodos([], 0).
contarNodos([X|Xs], S):- contarNodos(Xs, S2), S is S2 + X.

/* 
	Creamos el siguiente nivel del arbol con el numero de hijos correspondientes a cada nodo
*/
siguienteLista(0,_,C,C,L,L).
%siguienteLista(TL,R,C,Caux,L,SL):- Caux is 0 -> append(L,[0],NL), NC is 0, NTL is TL-1, siguienteLista(NTL,R,NC,Caux,NL,SL).
siguienteLista(TL,R,C,Caux,L,SL):- TL>0, C >= R -> 	between(1,R,R1), append(L,[R1],NL), NC is C-R1, 
													NTL is TL-1, siguienteLista(NTL,R1,NC,Caux,NL,SL).
siguienteLista(TL,R,C,_,L,SL):- TL>0, C is 0 -> append(L,[0],NL), NC is C-C, NTL is TL-1, 
												siguienteLista(NTL,R,NC,NC,NL,SL).
siguienteLista(TL,R,C,Caux,L,SL):- TL>0, C < R -> 	between(1,C,C1), append(L,[C1],NL), NC is C-C1, 
													NTL is TL-1, siguienteLista(NTL,R,NC,Caux,NL,SL).

/*
	Se toma una decision de acuerdo a la cantidad de nodos que se haya agregado al arbol
*/
decidir(Caux,R,NL,T,Esq):- Caux \= 0 -> formarEsqueleto(Caux,R,NL,T,Esq).
decidir(Caux,R,NL,T,Esq):- Caux is 0, siguienteLista(T,R,0,Caux,[],SL), append(NL,[SL],LF), 
										formarEsqueleto(0,R,LF,T,Esq).

/* 
	Formamos el esqueleto del arbol
*/
formarEsqueleto(0,_,L,_,L).
formarEsqueleto(N,R,L,TL,Esq):- N>0 -> siguienteLista(TL,R,N,Caux,[],SL), contarNodos(SL,T), 
									append(L,[SL],NL), decidir(Caux,R,NL,T,Esq).

/*
	Utilizamos el predicado correcto
*/
esqueleto(X,R,_):- X is 1, R>0 -> !, false.
esqueleto(X,R,_):- X is 1, R is 0 -> !.
esqueleto(X,R,Esq):- X>1 -> Nn is X-1, formarEsqueleto(Nn,R,[],1,Esq).

%%% GENERAR ARBOL BIEN ETIQUETADO A PARTIR DE ESQUELETO %%%

/*
	Contamos en los niveles del arbol
*/
contarListas([],0).
contarListas([L|R],N):- contarNodos(L,Nn), contarListas(R,Nl), N is Nn+Nl.

/*
	Obtenemos el primer elemento de una lista
*/
obtener_primero([C|R],X):- X is C.

/*
	Tomamos los elementos restantes de la lista
*/
tomar_elementos(_,[],[]).
tomar_elementos(N,L,S):- [C|R] = L, length(X, N), append(X, Y, C), S = Y.

/*
	Creamos las aristas para cada nodo verificando los etiquetamientos
*/
crear_aristas(0,_,Ln,La,Ln,La,_,[]):- !.
crear_aristas(N,Esq,Ln,La,LN,LA,Nn,Arist) :-	N > 0, NN is N-1, [R|Ni] = Esq, [C|RN] = R, tomar_elementos(C,Ni,NL),
												append([NL],T2,NE2), append([RN],NE2,NE1), member(Nod,Ln), member(Ar,La),
												delete(Ln,Nod,Lnaux), delete(La,Ar,Laaux), crear_aristas(C,Ni,Lnaux,Laaux,LN1,LA1,Nod,Arist1),
												crear_aristas(NN,NE1,LN1,LA1,LN2,LA2,Nn,Arist2).


/*
	Generamos el arbol a partir del esqueleto dado
*/
etiquetamiento([R|H],Arb) :- 	contarNodos(R,Nn), contarListas([R|H],Nl), NN is Nl +1, numlist(1,NN,Ln), member(Aux,Ln), 
								delete(Ln,NN,La), delete(Ln,Aux,Lnaux), crear_aristas(Nn,H,Lnaux,La,_,_,Aux,A), Arb = nodo(Aux,A).
								
%%% IMPRIMIR ARBOL %%% 

escribirLineas(0).
escribirLineas(X):- X>0 -> write("--------"), N is X-1, escribirLineas(N).

imprimir(nodo(_,[]),_).

imprimir(nodo(C,[arista(A,nodo(B,L))|NS]),X):- nl, escribirLineas(X), W is X + 1, write("("), write(C), write(")"), write("--"), write(A), write("--"), write("("), write(B), write(")"), imprimir(nodo(B,L),W), imprimir(nodo(C,NS),X).

describirEtiquetamiento(nodo(C,L)):- imprimir(nodo(C,L),0).