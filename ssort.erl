%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Algorithmen und Datenstrukturen                       %%
%% Praktikumsaufgabe 2 : naives vs komplexes Sortieren   %%
%%                                                       %%
%% bearbeitet von Helge Johannsen und Christian Stüber   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ssort).
-export([run_tests/0, insertionS/1, selectionS/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementierungen der einfachen Sortieralgorithmen    %%
%% Insertionsort und Selectionsort                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Sortiert die gegebene Liste.
%%
%% Signatur | insertionS: list → list
%%
insertionS(L) when is_list(L) -> insertionS([], L).
insertionS(Sorted, []) 		  -> reverse(Sorted);
insertionS(Sorted, [H|T]) 	  -> insertionS(insert(Sorted, H), T).

%%
%% Fuegt das Element an die korrekte Stelle fuer eine sortierte Liste ein.
%%
%% Signatur | insert: list x elem → list
%%
insert([], New) 							-> [New]; 							%% empty list
insert([H], New)   when H >= New 			-> [H, New]; 						%% list end
insert([H|T], New) when H < New 			-> [New, H|T]; 						%% list front
insert([H|T = [H2|_]], New) when H2 >= New  -> [H|insert(T, New)]; 				%% iterate while the next element is still smaller than new elem
insert([H|T = [H2|_]], New) when H2 < New 	-> [H, New|T]. 						%% insert now

%% ---------------------------------------------------------

%%
%% Sortiert die gegebene Liste.
%%
%% Signatur | selectionS: list → list
%%
selectionS(L) when is_list(L) -> selectionS([], L).
selectionS(Sorted, []) 		 -> reverse(Sorted);
selectionS(Sorted, Unsorted) ->
  Min = getMin(Unsorted),
  selectionS([Min|Sorted], delete(Min, Unsorted)).

%%
%% Gibt das kleinste Element der Liste zurueck.
%%
%% Signatur | getMin: list → elem
%%
getMin(L) -> getMin(nil, L).
getMin(Min, []) -> Min;
getMin(nil, [H|T]) -> getMin(H, T);
getMin(Min, [H|T]) when H < Min -> getMin(H, T);
getMin(Min, [_|T]) -> getMin(Min, T).

%%
%% Loescht das erste Element aus der Liste, dass gleich dem uebergebenen Element ist.
%%
%% Signatur | delete: elem x list → list
%%
delete(_, []) -> [];
delete(Elem, [Elem|T]) -> T;
delete(Elem, [H|T]) -> [H|delete(Elem, T)].

%%
%% Dreht die gegebene Liste um.
%%
%% Signatur | reverse: list → list
%%
reverse(L) -> reverse([], L).
reverse(Reversed, []) -> Reversed;
reverse(Reversed, [H|T]) -> reverse([H|Reversed], T).

%%%%%%%%%%%%%%%%
%% Unit-Tests %%
%%%%%%%%%%%%%%%%
test_insertionS() ->
  RESULT = (insertionS([]) == [])
    and (insertionS([1]) == [1])
    and (insertionS([5,4]) == [4,5])
    and (insertionS([4,5]) == [4,5])
    and (insertionS([4,4]) == [4,4])
    and (insertionS([4,6,5]) == [4,5,6])
    and (insertionS([4,3,6,2,5,1,7]) == [1,2,3,4,5,6,7])
    and (insertionS([-5,4,7,-1]) == [-5,-1,4,7]),
  io:format("test_insertionS: ~p~n", [RESULT]).

test_insert() ->
  RESULT = (insert([], 2) == [2])
    and (insert([1], 2) == [1, 2])
    and (insert([2], 1) == [1, 2])
    and (insert([1, 2], 1) == [1, 1, 2])
    and (insert([1, 2], 2) == [1, 2, 2])
    and (insert([1, 2], 3) == [1, 2, 3])
    and (insert([1, 3], 2) == [1, 2, 3]),
  io:format("test_insert: ~p~n", [RESULT]).

test_selectionS() ->
  RESULT = (selectionS([]) == [])
    and (selectionS([1]) == [1])
    and (selectionS([5,4]) == [4,5])
    and (selectionS([4,5]) == [4,5])
    and (selectionS([4,4]) == [4,4])
    and (selectionS([4,6,5]) == [4,5,6])
    and (selectionS([4,3,6,2,5,1,7]) == [1,2,3,4,5,6,7])
    and (selectionS([-5,4,7,-1]) == [-5,-1,4,7]),
  io:format("test_selectionS: ~p~n", [RESULT]).

test_getMin() ->
  RESULT = (getMin([]) == nil)
    and (getMin([1]) == 1)
    and (getMin([2, 1]) == 1)
    and (getMin([1, 2]) == 1)
    and (getMin([1, 2, 3]) == 1)
    and (getMin([3, 2, 1]) == 1)
    and (getMin([2, 1, 3]) == 1),
  io:format("test_getMin: ~p~n", [RESULT]).

test_delete() ->
  RESULT = (delete(1, []) == [])
    and (delete(1, [1]) == [])
    and (delete(1, [2, 1]) == [2])
    and (delete(1, [1, 2]) == [2])
    and (delete(1, [1, 2, 3]) == [2,3])
    and (delete(1, [3, 2, 1]) == [3,2])
    and (delete(1, [2, 1, 3]) == [2,3])
    and (delete(1, [1, 1, 2, 3]) == [1,2,3])
    and (delete(1, [3, 1, 2, 1]) == [3,2,1])
    and (delete(1, [2, 1, 1, 3]) == [2,1,3]),
  io:format("test_delete: ~p~n", [RESULT]).

%%%%%%%%%%%%%%%%%%%%%%
%%  Run Unit-Tests  %%
%%%%%%%%%%%%%%%%%%%%%%
run_tests() ->
  test_insert(),
  test_insertionS(),
  test_getMin(),
  test_delete(),
  test_selectionS().
