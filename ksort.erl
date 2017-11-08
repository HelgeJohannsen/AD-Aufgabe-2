%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Algorithmen und Datenstrukturen                       %%
%% Praktikumsaufgabe 2 : naives vs komplexes Sortieren   %%
%%                                                       %%
%% bearbeitet von Helge Johannsen und Christian Stüber   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ksort).
-export([run_tests/0, qsort/3, msort/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementierungen der komplexen Sortieralgorithmen    %%
%% Quicksort und Mergesort                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Sortiert die gegebene Liste.
%%
%% Signatur | qsort: atom x list x int → list
%%
qsort(_, [], _) -> [];
qsort(PivotMethod, L, SwitchNum) ->
  case switch(L, SwitchNum) of
    true -> ssort:insertionS(L);
    false ->
      Pivot = pivot(L, PivotMethod),
      {Smaller, Equal, Larger} = partition(Pivot, L),
      qsort(PivotMethod, Smaller, SwitchNum) ++ Equal ++ qsort(PivotMethod, Larger, SwitchNum)
  end.

%%
%% Partitioniert eine Liste in drei Teile, die jeweils kleiner, gleich oder groeßer als das Pivot-Element sind.
%%
%% Signatur | partition: int x list → {list, list, list}
%%
partition(Pivot, L) -> partition(Pivot, L, {[], [], []}).
partition(_, [], Parts) -> Parts;
partition(Pivot, [H|T], {Smaller, Equal, Larger}) ->
  if 	H < Pivot -> partition(Pivot, T, {[H|Smaller], Equal, Larger});
    H == Pivot -> partition(Pivot, T, {Smaller, [H|Equal], Larger});
    H > Pivot -> partition(Pivot, T, {Smaller, Equal, [H|Larger]})
  end.

%%
%% Gibt das Pivot-Element zurueck.
%%
%% Signatur | pivot: list x atom → elem
%%
pivot([], _) -> nil;
pivot([H], _) -> H;
pivot(L, PivotMethod) ->
  case PivotMethod of
    left -> lists:nth(1, L);
    middle -> lists:nth(round(length(L) / 2), L);
    right -> lists:last(L);
    median -> median({lists:nth(1, L), lists:nth(round(length(L) / 2), L), lists:last(L)});
    random -> lists:nth(rand:uniform(length(L)), L)
  end.

%%
%% Ermittelt ob eine gegebene Liste eine gegebene Mindestlaenge hat.
%%
%% Signatur | switch: list x int → boolean
%%
switch(L, SwitchNum) -> switch(0, L, SwitchNum).
switch(I, _, SwitchNum) when I > SwitchNum -> false;
switch(_, [], _) -> true;
switch(I, [_|T], SwitchNum) -> switch(I+1, T, SwitchNum).

%%
%% Gibt den Mittelwert von drei Werten zurueck.
%%
%% Signatur | median: {int, int, int} → int
%%
median({L, M, R}) when ((M =< L) and (L =< R)) or ((M >= L) and (L >= R)) -> L;
median({L, M, R}) when ((L =< M) and (M =< R)) or ((L >= M) and (M >= R)) -> M;
median({_, _, R}) -> R.

%% ---------------------------------------------------------

%%
%% Sortiert die gegebene Liste.
%%
%% Signatur | msort: list → list
%%
msort([])                -> [];
msort([H]) 			 	 -> [H];
msort(L) when is_list(L) ->
  {L1, L2} = split(round(length(L) / 2), L),
  merge(msort(L1), msort(L2)).

%%
%% Teilt die Liste in zwei Teil-Listen auf, es wird nach dem angegebene Index aufgeteilt.
%%
%% Signatur | split: int x list → {list, list}
%%
split(N, L) -> split([], N, L).
split(Before, 0, L) -> {reverse(Before), L};
split(Before, _, []) -> {reverse(Before), []};
split(Before, N, [H|T]) -> split([H|Before], N-1, T).

%%
%% Gibt die sortierte Liste zurueck, die aus zwei sortierten Listen geformt wird.
%%
%% Signatur | merge: list x list → list
%%
merge(L1, L2) -> merge([], L1, L2).
merge([], [], []) -> [];
merge(Sorted, [], []) -> reverse(Sorted);
merge(Sorted, [H|T], []) -> merge([H|Sorted], T, []);
merge(Sorted, [], [H|T]) -> merge([H|Sorted], [], T);
merge(Sorted, [H1|T1], [H2|T2]) ->
  case H1 < H2 of
    true -> merge([H1|Sorted], T1, [H2|T2]);
    false -> merge([H2|Sorted], [H1|T1], T2)
  end.

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
test_qsort() ->
  RESULT = (qsort(random, [], 0) == [])
    and (qsort(random, [1], 0) == [1])
    and (qsort(random, [5,4], 0) == [4,5])
    and (qsort(random, [4,5], 0) == [4,5])
    and (qsort(random, [4,4], 0) == [4,4])
    and (qsort(random, [4,6,5], 0) == [4,5,6])
    and (qsort(random, [4,3,6,2,5,1], 0) == [1,2,3,4,5,6])
    and (qsort(random, [-5,4,7,-1], 0) == [-5,-1,4,7])
    and (qsort(left, [], 0) == [])
    and (qsort(left, [1], 0) == [1])
    and (qsort(left, [5,4], 0) == [4,5])
    and (qsort(left, [4,5], 0) == [4,5])
    and (qsort(left, [4,4], 0) == [4,4])
    and (qsort(left, [4,6,5], 0) == [4,5,6])
    and (qsort(left, [4,3,6,2,5,1], 0) == [1,2,3,4,5,6])
    and (qsort(left, [-5,4,7,-1], 0) == [-5,-1,4,7])
    and (qsort(middle, [], 0) == [])
    and (qsort(middle, [1], 0) == [1])
    and (qsort(middle, [5,4], 0) == [4,5])
    and (qsort(middle, [4,5], 0) == [4,5])
    and (qsort(middle, [4,4], 0) == [4,4])
    and (qsort(middle, [4,6,5], 0) == [4,5,6])
    and (qsort(middle, [4,3,6,2,5,1], 0) == [1,2,3,4,5,6])
    and (qsort(middle, [-5,4,7,-1], 0) == [-5,-1,4,7])
    and (qsort(right, [], 0) == [])
    and (qsort(right, [1], 0) == [1])
    and (qsort(right, [5,4], 0) == [4,5])
    and (qsort(right, [4,5], 0) == [4,5])
    and (qsort(right, [4,4], 0) == [4,4])
    and (qsort(right, [4,6,5], 0) == [4,5,6])
    and (qsort(right, [4,3,6,2,5,1], 0) == [1,2,3,4,5,6])
    and (qsort(right, [-5,4,7,-1], 0) == [-5,-1,4,7])
    and (qsort(median, [], 0) == [])
    and (qsort(median, [1], 0) == [1])
    and (qsort(median, [5,4], 0) == [4,5])
    and (qsort(median, [4,5], 0) == [4,5])
    and (qsort(median, [4,4], 0) == [4,4])
    and (qsort(median, [4,6,5], 0) == [4,5,6])
    and (qsort(median, [4,3,6,2,5,1], 0) == [1,2,3,4,5,6])
    and (qsort(median, [-5,4,7,-1], 0) == [-5,-1,4,7]),
  io:format("test_qsort: ~p~n", [RESULT]).

test_pivot() ->
  RESULT = (pivot([1,2,3], left) == 1)
    and (pivot([1], left) == 1)
    and (pivot([], left) == nil)
    and (pivot([], middle) == nil)
    and (pivot([1], middle) == 1)
    and (pivot([1,2], middle) == 1)
    and (pivot([1,2,3], middle) == 2)
    and (pivot([1,2,3,4], middle) == 2)
    and (pivot([], right) == nil)
    and (pivot([1], right) == 1)
    and (pivot([1,2], right) == 2)
    and (pivot([], median) == nil)
    and (pivot([1], median) == 1)
    and (pivot([1,2], median) == 1)
    and (pivot([1, 1, 2], median) == 1)
    and (pivot([1, 2, 3], median) == 2)
    and (pivot([2, 1, 3], median) == 2)
    and (pivot([1, 3, 2], median) == 2)
    and (pivot([], random) == nil)
    and (pivot([1], random) == 1),
  R1 = pivot([1,2,3], random),
  R2 = pivot([1,2,3], random),
  R3 = pivot([1,2,3], random),
  L1 = [Elem || Elem <- [1,2,3], Elem == R1],
  L2 = [Elem || Elem <- [1,2,3], Elem == R2],
  L3 = [Elem || Elem <- [1,2,3], Elem == R3],
  RESULT2 = (RESULT and (length(L1) /= 0) and (length(L2) /= 0) and (length(L3) /= 0)),
  io:format("test_pivot: ~p~n", [RESULT2]).

test_switch() ->
  RESULT = (switch([1,2,3], 0) == false)
    and (switch([1,2,3], 1) == false)
    and (switch([1,2,3], 2) == false)
    and (switch([1,2,3], 3) == true)
    and (switch([1,2,3], 4) == true)
    and (switch([1], 0) == false)
    and (switch([1], 1) == true)
    and (switch([1], 2) == true)
    and (switch([], 0) == true)
    and (switch([], 1) == true),
  io:format("test_switch: ~p~n", [RESULT]).

test_median() ->
  RESULT = (median({1,2,3}) == 2)
    and (median({2,1,3}) == 2)
    and (median({3,1,2}) == 2)
    and (median({2,2,2}) == 2)
    and (median({1,2,2}) == 2)
    and (median({2,2,1}) == 2)
    and (median({2,1,2}) == 2),
  io:format("test_median: ~p~n", [RESULT]).

test_msort() ->
  RESULT = (msort([]) == [])
    and (msort([1]) == [1])
    and (msort([5,4]) == [4,5])
    and (msort([4,5]) == [4,5])
    and (msort([4,4]) == [4,4])
    and (msort([4,6,5]) == [4,5,6])
    and (msort([4,3,6,2,5,1]) == [1,2,3,4,5,6])
    and (msort([-5,4,7,-1]) == [-5,-1,4,7]),
  io:format("test_msort: ~p~n", [RESULT]).

test_split() ->
  RESULT = (split(0, []) == {[],[]})
    and (split(0, [1]) == {[],[1]})
    and (split(1, [1]) == {[1],[]})
    and (split(2, [1]) == {[1],[]})
    and (split(0, [5,4]) == {[],[5,4]})
    and (split(1, [5,4]) == {[5],[4]})
    and (split(2, [5,4]) == {[5,4],[]})
    and (split(1, [1,2,3]) == {[1],[2,3]})
    and (split(2, [1,2,3]) == {[1,2],[3]}),
  io:format("test_split: ~p~n", [RESULT]).

test_merge() ->
  RESULT = (merge([], []) == [])
    and (merge([1], []) == [1])
    and (merge([], [1]) == [1])
    and (merge([1], [1]) == [1, 1])
    and (merge([2], [1]) == [1, 2])
    and (merge([3, 4], [1, 2]) == [1, 2, 3, 4])
    and (merge([1, 2], [3, 4]) == [1, 2, 3, 4])
    and (merge([1, 3], [2, 4]) == [1, 2, 3, 4])
    and (merge([1, 3, 6], [2, 3, 4, 5, 6]) == [1, 2, 3, 3, 4, 5, 6, 6]),
  io:format("test_merge: ~p~n", [RESULT]).

%%%%%%%%%%%%%%%%%%%%%%
%%  Run Unit-Tests  %%
%%%%%%%%%%%%%%%%%%%%%%
run_tests() ->
  test_split(),
  test_merge(),
  test_msort(),
  test_pivot(),
  test_switch(),
  test_median(),
  test_qsort().
